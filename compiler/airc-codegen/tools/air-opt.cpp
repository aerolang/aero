#include "mlir/include/mlir/InitAllDialects.h"
#include "mlir/include/mlir/InitAllPasses.h"
#include "mlir/include/mlir/Tools/mlir-opt/MlirOptMain.h"

int main(int argc, char **argv) {
  mlir::DialectRegistry registry;
  mlir::registerAllDialects(registry);
  mlir::registerAllPasses();

  return mlir::asMainReturnCode(
    mlir::MlirOptMain(argc, argv, "AIR Pass Driver", registry));
}
