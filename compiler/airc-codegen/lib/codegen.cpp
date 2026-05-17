#include "compiler/airc-codegen/include/codegen.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRToLLVM.h"
#include "compiler/airc-codegen/include/MLIRGen.h"
#include "compiler/airc-codegen/schema/air_ast_generated.h"
#include "compiler/airc-codegen/src/lib.rs.h"

#include "lld/Common/Driver.h"
#include "mlir/Conversion/ArithToLLVM/ArithToLLVM.h"
#include "mlir/Conversion/ControlFlowToLLVM/ControlFlowToLLVM.h"
#include "mlir/Conversion/FuncToLLVM/ConvertFuncToLLVMPass.h"
#include "mlir/Conversion/ReconcileUnrealizedCasts/ReconcileUnrealizedCasts.h"
#include "mlir/Conversion/SCFToControlFlow/SCFToControlFlow.h"
#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/DialectRegistry.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"
#include "mlir/Pass/PassManager.h"
#include "mlir/Target/LLVMIR/Export.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"

LLD_HAS_DRIVER(elf)
LLD_HAS_DRIVER(macho)

#include <functional>
#include <mutex>
#include <string>
#include <vector>

namespace mlir {
void registerBuiltinDialectTranslation(DialectRegistry &);
void registerLLVMDialectTranslation(DialectRegistry &);
} // namespace mlir

using namespace mlir::air;
using namespace air_ast;

namespace airc::codegen {

//===----------------------------------------------------------------------===//
// Pass pipeline
//===----------------------------------------------------------------------===//

static bool lowerToLLVM(mlir::MLIRContext &context, mlir::ModuleOp module) {
  mlir::PassManager pm(&context);
  pm.addPass(createConvertAIRToFuncPass());
  pm.addPass(createConvertAIRToArithPass());
  pm.addPass(createConvertAIRToSCFPass());
  pm.addPass(createConvertAIRToLLVMPass());
  pm.addPass(mlir::createConvertFuncToLLVMPass());
  pm.addPass(mlir::createArithToLLVMConversionPass());
  pm.addPass(mlir::createSCFToControlFlowPass());
  pm.addPass(mlir::createConvertControlFlowToLLVMPass());
  pm.addPass(mlir::createReconcileUnrealizedCastsPass());
  if (failed(pm.run(module)))
    return false;
  return true;
}

//===----------------------------------------------------------------------===//
// Backend
//===----------------------------------------------------------------------===//

// Returns an empty string on success, or an error message on failure.
using ErrorStr = std::string;

static ErrorStr
writeToFile(const std::string &path,
            std::function<void(llvm::raw_fd_ostream &)> writeFn) {
  std::error_code ec;
  llvm::raw_fd_ostream file(path, ec, llvm::sys::fs::OF_None);
  if (ec)
    return "failed to open " + path + ": " + ec.message();
  writeFn(file);
  return {};
}

static ErrorStr emitObjectFile(llvm::Module &mod, const std::string &objPath) {
  llvm::Triple triple(llvm::sys::getDefaultTargetTriple());
  std::string errStr;
  const llvm::Target *target =
      llvm::TargetRegistry::lookupTarget(triple, errStr);
  if (!target)
    return "target lookup failed: " + errStr;

  llvm::TargetOptions opts;
  auto tm = std::unique_ptr<llvm::TargetMachine>(
      target->createTargetMachine(triple, "generic", "", opts, std::nullopt));
  mod.setDataLayout(tm->createDataLayout());

  std::error_code ec;
  llvm::raw_fd_ostream dest(objPath, ec, llvm::sys::fs::OF_None);
  if (ec)
    return "failed to open " + objPath + ": " + ec.message();

  llvm::legacy::PassManager pass;
  if (tm->addPassesToEmitFile(pass, dest, nullptr,
                              llvm::CodeGenFileType::ObjectFile))
    return "target cannot emit object files";
  pass.run(mod);
  dest.flush();
  return {};
}

static std::once_flag lldInitFlag;
static std::mutex lldMutex;

#ifdef __APPLE__

static llvm::Expected<std::string> xcrunQuery(const char *flag) {
  std::string cmd = std::string("xcrun ") + flag;
  FILE *pipe = popen(cmd.c_str(), "r");
  if (!pipe)
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   std::string("failed to run xcrun ") + flag);
  std::string result;
  char buf[256];
  while (fgets(buf, sizeof(buf), pipe))
    result += buf;
  if (pclose(pipe) != 0)
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   std::string("`xcrun ") + flag + "` failed");
  llvm::StringRef trimmed = llvm::StringRef(result).trim();
  if (trimmed.empty())
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   std::string("`xcrun ") + flag +
                                       "` returned empty output");
  return trimmed.str();
}

static llvm::Expected<std::string> swVersQuery() {
  FILE *pipe = popen("sw_vers -productVersion", "r");
  if (!pipe)
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   "failed to run sw_vers");
  std::string result;
  char buf[256];
  while (fgets(buf, sizeof(buf), pipe))
    result += buf;
  if (pclose(pipe) != 0)
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   "sw_vers failed");
  llvm::StringRef trimmed = llvm::StringRef(result).trim();
  if (trimmed.empty())
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   "sw_vers returned empty output");
  return trimmed.str();
}

struct MacOSSDK {
  std::string sysroot;
  std::string sdkVersion;
  std::string osVersion;
};

static llvm::Expected<MacOSSDK> findMacOSSDK() {
  auto sysroot = xcrunQuery("--show-sdk-path");
  if (!sysroot)
    return sysroot.takeError();
  auto sdkVersion = xcrunQuery("--show-sdk-version");
  if (!sdkVersion)
    return sdkVersion.takeError();
  auto osVersion = swVersQuery();
  if (!osVersion)
    return osVersion.takeError();
  MacOSSDK sdk;
  sdk.sysroot = std::move(*sysroot);
  sdk.sdkVersion = std::move(*sdkVersion);
  sdk.osVersion = std::move(*osVersion);
  return sdk;
}

static ErrorStr linkWithRuntime(const std::string &objPath,
                                const std::string &outputStr,
                                const std::string &runtimePath,
                                const std::string &builtinsPath,
                                const MacOSSDK &sdk) {
  if (!llvm::sys::fs::exists(runtimePath))
    return "runtime library not found at " + runtimePath;
  if (!llvm::sys::fs::exists(builtinsPath))
    return "builtins library not found at " + builtinsPath;

  std::call_once(lldInitFlag, [] { llvm::CrashRecoveryContext::Enable(); });

  llvm::Triple triple(llvm::sys::getDefaultTargetTriple());
  // LLD MachO uses "arm64" not "aarch64".
  std::string arch = triple.getArch() == llvm::Triple::aarch64
                         ? "arm64"
                         : triple.getArchName().str();
  std::string forceLoad = "-force_load";
  std::vector<const char *> args = {
      "ld64.lld",
      "-arch",
      arch.c_str(),
      "-platform_version",
      "macos",
      sdk.osVersion.c_str(),
      sdk.sdkVersion.c_str(),
      "-syslibroot",
      sdk.sysroot.c_str(),
      objPath.c_str(),
      "-o",
      outputStr.c_str(),
      "-e",
      "_aero$entrypoint",
      forceLoad.c_str(),
      runtimePath.c_str(),
      forceLoad.c_str(),
      builtinsPath.c_str(),
      "-lSystem",
  };
  lld::Result r;
  {
    std::lock_guard<std::mutex> lock(lldMutex);
    r = lld::lldMain(args, llvm::outs(), llvm::errs(),
                     {{lld::Darwin, &lld::macho::link}});
  }
  if (r.retCode != 0)
    return "linking failed";
  return {};
}

#else

static ErrorStr linkWithRuntime(const std::string &objPath,
                                const std::string &outputStr,
                                const std::string &runtimePath,
                                const std::string &builtinsPath) {
  if (!llvm::sys::fs::exists(runtimePath))
    return "runtime library not found at " + runtimePath;
  if (!llvm::sys::fs::exists(builtinsPath))
    return "builtins library not found at " + builtinsPath;

  std::call_once(lldInitFlag, [] { llvm::CrashRecoveryContext::Enable(); });

  std::string forceLoad = "--whole-archive";
  std::string noForceLoad = "--no-whole-archive";
  std::vector<const char *> args = {
      "ld.lld",
      objPath.c_str(),
      "-o",
      outputStr.c_str(),
      "-e",
      "aero$entrypoint",
      forceLoad.c_str(),
      runtimePath.c_str(),
      builtinsPath.c_str(),
      noForceLoad.c_str(),
  };
  lld::Result r;
  {
    std::lock_guard<std::mutex> lock(lldMutex);
    r = lld::lldMain(args, llvm::outs(), llvm::errs(),
                     {{lld::Gnu, &lld::elf::link}});
  }
  if (r.retCode != 0)
    return "linking failed";
  return {};
}

#endif

//===----------------------------------------------------------------------===//
// Entry point
//===----------------------------------------------------------------------===//

static CompileResult
makeResult(bool ok,
           std::vector<std::tuple<uint32_t, uint32_t, std::string>> diagData) {
  CompileResult result;
  result.ok = ok;
  for (auto &[line, col, msg] : diagData) {
    Diag d;
    d.line = line;
    d.col = col;
    d.message = rust::String(msg);
    result.diags.push_back(std::move(d));
  }
  return result;
}

CompileResult compile_air_bytes(rust::Slice<const uint8_t> msg,
                                rust::Str output_path, rust::Str support_path) {
  std::vector<std::tuple<uint32_t, uint32_t, std::string>> diags;

  auto fail = [&](uint32_t line, uint32_t col, std::string message) {
    diags.emplace_back(line, col, std::move(message));
    return makeResult(false, std::move(diags));
  };

  flatbuffers::Verifier verifier(msg.data(), msg.size());
  if (!VerifySourceBuffer(verifier))
    return fail(0, 0, "invalid Source buffer");

  const Source *source = GetSource(msg.data());

  std::string outputStr(output_path.data(), output_path.size());
  std::string supportDir(support_path.data(), support_path.size());
  std::string runtimePath = supportDir + "/libruntime.a";
  std::string builtinsPath = supportDir + "/libbuiltins.a";
  std::string filename = source->filename() ? source->filename()->str() : "";

  const MainDef *mainDef = nullptr;
  if (source->defs()) {
    for (const auto *def : *source->defs()) {
      if (def && def->data_type() == DefData::MainDef) {
        const auto *md = def->data_as_MainDef();
        if (md && md->is_pub()) {
          mainDef = md;
          break;
        }
      }
    }
  }
  if (!mainDef)
    return fail(0, 0, "no public main definition found");

  mlir::DialectRegistry registry;
  mlir::registerBuiltinDialectTranslation(registry);
  mlir::registerLLVMDialectTranslation(registry);

  mlir::MLIRContext context;
  context.appendDialectRegistry(registry);
  context.loadAllAvailableDialects();
  context.getOrLoadDialect<AIRDialect>();
  context.getOrLoadDialect<mlir::arith::ArithDialect>();
  context.getOrLoadDialect<mlir::func::FuncDialect>();
  context.getOrLoadDialect<mlir::LLVM::LLVMDialect>();

  context.getDiagEngine().registerHandler(
      [&diags](mlir::Diagnostic &diag) -> mlir::LogicalResult {
        if (diag.getSeverity() != mlir::DiagnosticSeverity::Error)
          return mlir::failure();
        uint32_t line = 0, col = 0;
        if (auto fileLoc =
                mlir::dyn_cast<mlir::FileLineColLoc>(diag.getLocation())) {
          line = fileLoc.getLine();
          col = fileLoc.getColumn();
        }
        std::string msg;
        llvm::raw_string_ostream os(msg);
        os << diag;
        diags.emplace_back(line, col, std::move(msg));
        return mlir::success();
      });

  auto moduleRef = mlirGen(context, source, mainDef, filename);
  if (!moduleRef)
    return makeResult(false, std::move(diags));

  mlir::ModuleOp module = moduleRef.get();

  if (failed(mlir::verify(module)))
    return fail(0, 0, "module verification failed");

  if (auto err = writeToFile(outputStr + ".air.mlir",
                             [&](llvm::raw_fd_ostream &f) { module.print(f); });
      !err.empty())
    return fail(0, 0, err);

  if (!lowerToLLVM(context, module))
    return makeResult(false, std::move(diags));

  if (auto err = writeToFile(outputStr + ".llvm.mlir",
                             [&](llvm::raw_fd_ostream &f) { module.print(f); });
      !err.empty())
    return fail(0, 0, err);

  llvm::LLVMContext llvmContext;
  auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);
  if (!llvmModule)
    return fail(0, 0, "failed to translate to LLVM IR");
  llvmModule->setTargetTriple(
      llvm::Triple(llvm::sys::getDefaultTargetTriple()));

  if (auto err = writeToFile(
          outputStr + ".ll",
          [&](llvm::raw_fd_ostream &f) { llvmModule->print(f, nullptr); });
      !err.empty())
    return fail(0, 0, err);

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();

  std::string objPath = outputStr + ".o";
  if (auto err = emitObjectFile(*llvmModule, objPath); !err.empty())
    return fail(0, 0, err);

#ifdef __APPLE__
  auto sdk = findMacOSSDK();
  if (!sdk)
    return fail(0, 0,
                "could not find macOS SDK: " + llvm::toString(sdk.takeError()));
  if (auto err =
          linkWithRuntime(objPath, outputStr, runtimePath, builtinsPath, *sdk);
      !err.empty())
    return fail(0, 0, err);
#else
  if (auto err = linkWithRuntime(objPath, outputStr, runtimePath, builtinsPath);
      !err.empty())
    return fail(0, 0, err);
#endif

  return makeResult(true, {});
}

} // namespace airc::codegen
