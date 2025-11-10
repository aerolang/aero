#include "compiler/airc-codegen/include/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/AIR/AIRToLLVM.h"
#include "mlir/include/mlir/InitAllDialects.h"
#include "mlir/include/mlir/InitAllPasses.h"
#include "mlir/include/mlir/Tools/mlir-opt/MlirOptMain.h"

int main(int argc, char **argv) {
  mlir::DialectRegistry registry;

  // Register AIR dialect
  registry.insert<mlir::air::AIRDialect>();

  // Register all standard MLIR dialects
  mlir::registerAllDialects(registry);

  // Register all standard passes
  mlir::registerAllPasses();

  // Register AIR to LLVM conversion pass
  mlir::air::registerAIRToLLVMPasses();

  return mlir::asMainReturnCode(
    mlir::MlirOptMain(argc, argv, "AIR Pass Driver", registry));
}
