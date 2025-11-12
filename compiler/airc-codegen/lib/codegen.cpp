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

void compile_air_ast(rust::Vec<FuncInfo> funcs, rust::Str output_path, rust::Str runtime_path) {
    std::string outputStr(output_path.data(), output_path.size());
    std::string runtimePath(runtime_path.data(), runtime_path.size());

    if (funcs.empty()) {
        std::cerr << "No functions to compile" << std::endl;
        return;
    }

    // Find the main function (must be pub)
    std::string mainFuncName;
    std::string mainLogMessage;
    bool foundMain = false;

    for (const auto& func : funcs) {
        std::string funcName(func.name.data(), func.name.size());
        if (funcName == "main" && func.is_pub) {
            foundMain = true;
            mainFuncName = funcName;
            mainLogMessage = std::string(func.log_message.data(), func.log_message.size());
            break;
        }
    }

    if (!foundMain) {
        std::cerr << "No public main function found" << std::endl;
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

    // Create function type: () -> void
    auto voidType = mlir::air::VoidType::get(&context);
    auto funcType = builder.getFunctionType({}, {voidType});

    // Create AIR function
    auto func = builder.create<mlir::air::FuncOp>(
        loc, builder.getStringAttr(mainFuncName), mlir::TypeAttr::get(funcType));

    // Create function body - manually add a region and block
    auto& bodyRegion = func.getBody();
    auto* entryBlock = builder.createBlock(&bodyRegion);
    builder.setInsertionPointToStart(entryBlock);

    // Create string constant
    auto strType = mlir::air::StrType::get(&context);
    auto strAttr = builder.getStringAttr(mainLogMessage);
    auto constOp = builder.create<mlir::air::ConstantOp>(
        loc, strType, strAttr);

    // Create log operation
    builder.create<mlir::air::LogOp>(loc, constOp.getResult());

    // Create return operation (no operands for void return)
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

    std::vector<llvm::StringRef> linkArgs = {
        "clang",
        objPath,
        "-o", outputStr,
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
