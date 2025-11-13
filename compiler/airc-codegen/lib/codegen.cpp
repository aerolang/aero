#include "compiler/airc-codegen/src/lib.rs.h"
#include "compiler/airc-codegen/include/codegen.h"
#include "compiler/airc-codegen/include/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/AIR/AIROps.h"
#include "compiler/airc-codegen/include/AIR/AIRToLLVM.h"
#include "compiler/airc-codegen/include/AIR/AIRTypes.h"

#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/DialectRegistry.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"
#include "mlir/Pass/PassManager.h"
#include "mlir/Target/LLVMIR/Export.h"

#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

// Forward declare translation registration functions
// These are defined in the ToLLVMIRTranslationRegistration library
namespace mlir {
void registerBuiltinDialectTranslation(DialectRegistry &);
void registerLLVMDialectTranslation(DialectRegistry &);
}

namespace airc {
namespace codegen {

void compile_air_ast(rust::Vec<SourceData> sources, rust::Str output_path, rust::Str runtime_path) {
    std::string outputStr(output_path.data(), output_path.size());
    std::string runtimePath(runtime_path.data(), runtime_path.size());

    if (sources.empty()) {
        std::cerr << "No sources to compile" << std::endl;
        return;
    }

    // Find the main definition (must be pub) across all sources
    const MainDefData* mainDef = nullptr;

    for (const auto& source : sources) {
        for (const auto& def : source.main_defs) {
            if (def.is_pub) {
                mainDef = &def;
                break;
            }
        }
        if (mainDef) break;
    }

    if (!mainDef) {
        std::cerr << "No public main definition found" << std::endl;
        return;
    }

    // Create MLIR context and register dialects
    mlir::DialectRegistry registry;
    mlir::registerBuiltinDialectTranslation(registry);
    mlir::registerLLVMDialectTranslation(registry);

    mlir::MLIRContext context(registry);
    context.loadAllAvailableDialects();
    context.getOrLoadDialect<mlir::air::AIRDialect>();
    context.getOrLoadDialect<mlir::LLVM::LLVMDialect>();

    // Create module
    mlir::OpBuilder builder(&context);
    auto loc = builder.getUnknownLoc();
    auto module = mlir::ModuleOp::create(loc);

    // Build AIR IR
    builder.setInsertionPointToEnd(module.getBody());

    // Create function type for the entrypoint: () -> void (in AIR, will add i32 return in LLVM)
    auto voidType = mlir::air::VoidType::get(&context);
    auto funcType = builder.getFunctionType({}, {voidType});

    // Create AIR function for the entrypoint
    auto func = builder.create<mlir::air::FuncOp>(
        loc, builder.getStringAttr("aero$entrypoint"), mlir::TypeAttr::get(funcType));

    // Create function body - manually add a region and block
    auto& bodyRegion = func.getBody();
    auto* entryBlock = builder.createBlock(&bodyRegion);
    builder.setInsertionPointToStart(entryBlock);

    // Helper to process an expression and generate MLIR ops
    auto processExpr = [&](const ExprData& expr) {
        std::string kind(expr.kind.data(), expr.kind.size());

        if (kind == "call") {
            std::string callee(expr.callee.data(), expr.callee.size());

            if (callee == "log") {
                // Extract the string argument
                if (!expr.args.empty()) {
                    const auto& arg = expr.args[0];
                    std::string argKind(arg.kind.data(), arg.kind.size());

                    if (argKind == "str") {
                        std::string strValue(arg.value.data(), arg.value.size());
                        auto strType = mlir::air::StrType::get(&context);
                        auto strAttr = builder.getStringAttr(strValue);
                        auto constOp = builder.create<mlir::air::ConstantOp>(
                            loc, strType, strAttr);
                        builder.create<mlir::air::LogOp>(loc, constOp.getResult());
                    }
                }
            }
        }
        // For simple expressions, we don't need to generate ops unless they're used
    };

    // Process all assigns (side effects like discarded log calls)
    for (const auto& assign : mainDef->assigns) {
        processExpr(assign.expr);
    }

    // Process the result expression
    processExpr(mainDef->result);

    // Return void (the LLVM conversion will add the i32 return)
    builder.create<mlir::air::ReturnOp>(loc, mlir::Value());

    // Verify the module
    if (failed(mlir::verify(module))) {
        std::cerr << "Module verification failed" << std::endl;
        module.dump();
        return;
    }

    std::cout << "Generated AIR MLIR:" << std::endl;
    module.dump();

    // Run conversion pass: AIR -> LLVM dialect
    mlir::PassManager pm(&context);
    pm.addPass(mlir::air::createConvertAIRToLLVMPass());

    if (failed(pm.run(module))) {
        std::cerr << "Pass manager failed" << std::endl;
        return;
    }

    std::cout << "\nAfter AIR->LLVM conversion:" << std::endl;
    module.dump();

    // Translate MLIR to LLVM IR
    llvm::LLVMContext llvmContext;
    auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);

    if (!llvmModule) {
        std::cerr << "Failed to translate to LLVM IR" << std::endl;
        return;
    }

    std::cout << "\nGenerated LLVM IR:" << std::endl;
    llvmModule->print(llvm::outs(), nullptr);

    // Write LLVM IR to file
    std::string llPath = outputStr + ".ll";
    std::error_code ec;
    llvm::raw_fd_ostream llFile(llPath, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "Failed to open " << llPath << ": " << ec.message() << std::endl;
        return;
    }
    llvmModule->print(llFile, nullptr);
    llFile.close();

    std::cout << "\nWrote LLVM IR to " << llPath << std::endl;

    // Compile LLVM IR to object file using clang
    std::string objPath = outputStr + ".o";
    std::vector<llvm::StringRef> clangArgs = {
        "clang",
        "-c",
        llPath,
        "-o", objPath
    };

    std::string errMsg;
    auto clang = llvm::sys::findProgramByName("clang");
    if (!clang) {
        std::cerr << "clang not found" << std::endl;
        return;
    }

    int result = llvm::sys::ExecuteAndWait(
        *clang, clangArgs, std::nullopt, {}, 0, 0, &errMsg);

    if (result != 0) {
        std::cerr << "clang failed: " << errMsg << std::endl;
        return;
    }

    std::cout << "Compiled to object file: " << objPath << std::endl;

    // Use the provided runtime library path
    if (!llvm::sys::fs::exists(runtimePath)) {
        std::cerr << "Error: Could not find air_runtime library at " << runtimePath << std::endl;
        return;
    }

    // On macOS/Darwin, the linker adds an extra underscore prefix to symbols
    // So aero$entrypoint becomes _aero$entrypoint in the object file
    std::string entrypoint;
#ifdef __APPLE__
    entrypoint = "_aero$entrypoint";
#else
    entrypoint = "aero$entrypoint";
#endif

    std::vector<llvm::StringRef> linkArgs = {
        "clang",
        objPath,
        "-o", outputStr,
        "-e", entrypoint,
        runtimePath
    };

    result = llvm::sys::ExecuteAndWait(
        *clang, linkArgs, std::nullopt, {}, 0, 0, &errMsg);

    if (result != 0) {
        std::cerr << "Linking failed: " << errMsg << std::endl;
        std::cerr << "Try running manually: clang " << objPath << " -o " << outputStr << " " << runtimePath << std::endl;
        return;
    }

    std::cout << "Successfully compiled to: " << outputStr << std::endl;
}

}  // namespace codegen
}  // namespace airc
