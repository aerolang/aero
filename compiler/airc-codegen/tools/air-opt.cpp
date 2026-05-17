#include "compiler/airc-codegen/include/Dialect/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRToLLVM.h"
#include "mlir/include/mlir/InitAllDialects.h"
#include "mlir/include/mlir/InitAllPasses.h"
#include "mlir/include/mlir/Tools/mlir-opt/MlirOptMain.h"

int main(int argc, char **argv) {
  mlir::DialectRegistry registry;
  registry.insert<mlir::air::AIRDialect>();

  mlir::registerAllDialects(registry);
  mlir::registerAllPasses();

  mlir::air::registerAIRToLLVMPasses();

  return mlir::asMainReturnCode(
      mlir::MlirOptMain(argc, argv, "AIR Pass Driver", registry));
}
