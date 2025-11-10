#ifndef AIR_TO_LLVM_TD
#define AIR_TO_LLVM_TD

include "mlir/Pass/PassBase.td"

def ConvertAIRToLLVM : Pass<"convert-air-to-llvm"> {
  let summary = "Convert AIR dialect to LLVM dialect";
  let description = [{
    This pass converts AIR dialect operations to LLVM dialect operations,
    enabling code generation to native executables.

    Conversions:
    - air.func -> llvm.func
    - air.log -> llvm.call @air_log
    - air.constant (string) -> llvm.mlir.global + llvm.mlir.addressof
    - air.return -> llvm.return
  }];

  let dependentDialects = [
    "mlir::LLVM::LLVMDialect"
  ];

  let constructor = "mlir::air::createConvertAIRToLLVMPass()";
}

#endif // AIR_TO_LLVM_TD
