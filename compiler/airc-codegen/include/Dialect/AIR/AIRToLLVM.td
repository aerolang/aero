#ifndef AIR_TO_LLVM_TD
#define AIR_TO_LLVM_TD

include "mlir/Pass/PassBase.td"

def ConvertAIRToArith : Pass<"convert-air-to-arith"> {
  let summary = "Convert AIR arithmetic and comparison ops to the arith dialect";
  let description = [{
    Converts air.add, air.sub, air.mul, air.div, and air.icmp to their
    arith dialect equivalents (arith.addi, arith.subi, etc.).
  }];

  let dependentDialects = [
    "mlir::arith::ArithDialect"
  ];

  let constructor = "mlir::air::createConvertAIRToArithPass()";
}

def ConvertAIRToFunc : Pass<"convert-air-to-func"> {
  let summary = "Convert AIR function/call ops to the func dialect";
  let description = [{
    Converts air.func_def, air.main_def, air.call, and air.return to their
    func dialect equivalents (func.func, func.call, func.return). After this
    pass, the standard ConvertFuncToLLVM pass handles the rest.
  }];

  let dependentDialects = [
    "mlir::func::FuncDialect"
  ];

  let constructor = "mlir::air::createConvertAIRToFuncPass()";
}

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

def ConvertAIRToSCF : Pass<"convert-air-to-scf"> {
  let summary = "Convert AIR structured control flow to SCF dialect";
  let description = [{
    Converts air.if/air.yield to scf.if/scf.yield, enabling the standard
    SCFToControlFlow and ControlFlowToLLVM lowering passes to handle branching.
  }];

  let dependentDialects = [
    "mlir::scf::SCFDialect"
  ];

  let constructor = "mlir::air::createConvertAIRToSCFPass()";
}

#endif // AIR_TO_LLVM_TD
