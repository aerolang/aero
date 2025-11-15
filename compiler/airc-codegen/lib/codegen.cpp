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

    // Symbol table for tracking variables and their MLIR values
    std::map<std::string, mlir::Value> symbolTable;

    // Helper to convert type string to MLIR type
    auto getAIRType = [&](const std::string& tyStr) -> mlir::Type {
        if (tyStr == "Str") return mlir::air::StrType::get(&context);
        if (tyStr == "Int") return mlir::air::IntType::get(&context);
        return mlir::air::VoidType::get(&context);
    };

    // Helper to process an expression and return its MLIR value
    std::function<mlir::Value(const ExprData&)> processExpr;
    processExpr = [&](const ExprData& expr) -> mlir::Value {
        std::string kind(expr.kind.data(), expr.kind.size());

        if (kind == "simple") {
            const auto& simple = expr.simple;
            std::string simpleKind(simple.kind.data(), simple.kind.size());
            std::string simpleValue(simple.value.data(), simple.value.size());

            if (simpleKind == "str") {
                auto strType = mlir::air::StrType::get(&context);
                auto strAttr = builder.getStringAttr(simpleValue);
                return builder.create<mlir::air::ConstantOp>(loc, strType, strAttr).getResult();
            } else if (simpleKind == "varname") {
                // Look up variable in symbol table
                auto it = symbolTable.find(simpleValue);
                if (it != symbolTable.end()) {
                    return it->second;
                }
                std::cerr << "Undefined variable: %" << simpleValue << std::endl;
                return nullptr;
            } else if (simpleKind == "void") {
                return nullptr;
            }
        } else if (kind == "call") {
            std::string callee(expr.callee.data(), expr.callee.size());

            // Evaluate arguments
            std::vector<mlir::Value> argValues;
            for (const auto& arg : expr.args) {
                ExprData argExpr;
                argExpr.kind = "simple";
                argExpr.simple = arg;
                argExpr.callee = "";
                auto argValue = processExpr(argExpr);
                if (argValue) {
                    argValues.push_back(argValue);
                }
            }

            // Determine the function name to call
            std::string funcName;
            mlir::TypeRange resultTypes;
            if (callee == "log") {
                // Map builtin 'log' to runtime function
                funcName = "runtime$log";
                resultTypes = mlir::TypeRange{}; // void return
            } else if (callee == "str-concat") {
                // Map builtin 'str-concat' to runtime function
                funcName = "runtime$str_concat";
                // Returns a string
                auto strType = mlir::air::StrType::get(&context);
                resultTypes = mlir::TypeRange{strType};
            } else if (callee == "free") {
                // Map builtin 'free' to runtime function
                funcName = "runtime$free";
                resultTypes = mlir::TypeRange{}; // void return
            } else if (!callee.empty() && callee[0] == '$') {
                // User-defined function call - strip the $
                funcName = callee.substr(1);
                resultTypes = mlir::TypeRange{}; // TODO: handle non-void returns
            } else {
                // Unknown callee
                return nullptr;
            }

            // Create function call
            auto funcRef = mlir::FlatSymbolRefAttr::get(&context, funcName);
            auto callOp = builder.create<mlir::air::CallOp>(
                loc,
                resultTypes,
                funcRef,
                argValues
            );

            // Return the result value if there is one
            if (callOp.getNumResults() > 0) {
                return callOp.getResult();
            }
            return nullptr;
        }

        return nullptr;
    };

    // First, generate all user-defined functions
    for (const auto& source : sources) {
        for (const auto& funcDef : source.func_defs) {
            std::string funcName(funcDef.name.data(), funcDef.name.size());

            // Convert parameter types
            std::vector<mlir::Type> paramTypes;
            for (const auto& param : funcDef.params) {
                std::string tyStr(param.ty.data(), param.ty.size());
                paramTypes.push_back(getAIRType(tyStr));
            }

            // Return type is always void for now
            auto voidType = mlir::air::VoidType::get(&context);
            auto funcType = builder.getFunctionType(paramTypes, {voidType});

            // Create AIR function
            auto func = builder.create<mlir::air::FuncOp>(
                loc, builder.getStringAttr(funcName), mlir::TypeAttr::get(funcType));

            // Create function body
            auto& bodyRegion = func.getBody();
            auto* entryBlock = builder.createBlock(&bodyRegion);

            // Add block arguments for parameters
            for (size_t i = 0; i < funcDef.params.size(); ++i) {
                entryBlock->addArgument(paramTypes[i], loc);
            }

            builder.setInsertionPointToStart(entryBlock);

            // Clear symbol table for new function
            symbolTable.clear();

            // Map parameters to block arguments
            for (size_t i = 0; i < funcDef.params.size(); ++i) {
                std::string paramName(funcDef.params[i].name.data(), funcDef.params[i].name.size());
                symbolTable[paramName] = entryBlock->getArgument(i);
            }

            // Process assignments
            for (const auto& assign : funcDef.assigns) {
                std::string varName(assign.var_name.data(), assign.var_name.size());
                auto value = processExpr(assign.expr);
                if (value) {
                    symbolTable[varName] = value;
                }
            }

            // Process result expression
            processExpr(funcDef.result);

            // Return void
            builder.create<mlir::air::ReturnOp>(loc, mlir::Value());

            // Reset insertion point for next function
            builder.setInsertionPointToEnd(module.getBody());
        }
    }

    // Now generate the entrypoint function
    auto voidType = mlir::air::VoidType::get(&context);
    auto funcType = builder.getFunctionType({}, {voidType});

    auto entrypointFunc = builder.create<mlir::air::FuncOp>(
        loc, builder.getStringAttr("aero$entrypoint"), mlir::TypeAttr::get(funcType));

    auto& bodyRegion = entrypointFunc.getBody();
    auto* entryBlock = builder.createBlock(&bodyRegion);
    builder.setInsertionPointToStart(entryBlock);

    // Clear symbol table for main
    symbolTable.clear();

    // Process main's assignments
    for (const auto& assign : mainDef->assigns) {
        std::string varName(assign.var_name.data(), assign.var_name.size());
        auto value = processExpr(assign.expr);
        if (value) {
            symbolTable[varName] = value;
        }
    }

    // Process main's result expression
    processExpr(mainDef->result);

    // Return void (the LLVM conversion will add the i32 return)
    builder.create<mlir::air::ReturnOp>(loc, mlir::Value());

    // Verify the module
    if (failed(mlir::verify(module))) {
        std::cerr << "Module verification failed" << std::endl;
        module.dump();
        return;
    }

    // Write AIR MLIR to file
    std::string airMlirPath = outputStr + ".air.mlir";
    std::error_code ec;
    llvm::raw_fd_ostream airMlirFile(airMlirPath, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "Failed to open " << airMlirPath << ": " << ec.message() << std::endl;
        return;
    }
    module.print(airMlirFile);
    airMlirFile.close();

    // Run conversion pass: AIR -> LLVM dialect
    mlir::PassManager pm(&context);
    pm.addPass(mlir::air::createConvertAIRToLLVMPass());

    if (failed(pm.run(module))) {
        std::cerr << "Pass manager failed" << std::endl;
        return;
    }

    // Write LLVM dialect MLIR to file
    std::string llvmMlirPath = outputStr + ".llvm.mlir";
    llvm::raw_fd_ostream llvmMlirFile(llvmMlirPath, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "Failed to open " << llvmMlirPath << ": " << ec.message() << std::endl;
        return;
    }
    module.print(llvmMlirFile);
    llvmMlirFile.close();

    // Translate MLIR to LLVM IR
    llvm::LLVMContext llvmContext;
    auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);

    if (!llvmModule) {
        std::cerr << "Failed to translate to LLVM IR" << std::endl;
        return;
    }

    // Write LLVM IR to file
    std::string llPath = outputStr + ".ll";
    llvm::raw_fd_ostream llFile(llPath, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "Failed to open " << llPath << ": " << ec.message() << std::endl;
        return;
    }
    llvmModule->print(llFile, nullptr);
    llFile.close();

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
