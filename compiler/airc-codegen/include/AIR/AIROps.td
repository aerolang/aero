#ifndef AIR_OPS_TD
#define AIR_OPS_TD

include "compiler/airc-codegen/include/AIR/AIRDialect.td"
include "compiler/airc-codegen/include/AIR/AIRTypes.td"
include "mlir/IR/OpBase.td"
include "mlir/Interfaces/SideEffectInterfaces.td"

// Base class for AIR operations
class AIR_Op<string mnemonic, list<Trait> traits = []> :
    Op<AIR_Dialect, mnemonic, traits>;

//===----------------------------------------------------------------------===//
// FuncOp - Simplified without FunctionOpInterface for now
//===----------------------------------------------------------------------===//

def AIR_FuncOp : AIR_Op<"func", [IsolatedFromAbove]> {
  let summary = "AIR function operation";
  let description = [{
    Defines a function in AIR. Simplified version without FunctionOpInterface.
  }];

  let arguments = (ins
    SymbolNameAttr:$sym_name,
    TypeAttrOf<FunctionType>:$function_type
  );

  let regions = (region AnyRegion:$body);
}

//===----------------------------------------------------------------------===//
// LogOp
//===----------------------------------------------------------------------===//

def AIR_LogOp : AIR_Op<"log", []> {
  let summary = "AIR log operation";
  let description = [{
    Logs a value to standard output.
  }];

  let arguments = (ins AIR_AnyType:$value);

  let assemblyFormat = "$value attr-dict `:` type($value)";
}

//===----------------------------------------------------------------------===//
// ConstantOp
//===----------------------------------------------------------------------===//

def AIR_ConstantOp : AIR_Op<"constant", [Pure, ConstantLike]> {
  let summary = "AIR constant operation";
  let description = [{
    Produces a constant value.
  }];

  let arguments = (ins AnyAttr:$value);
  let results = (outs AIR_AnyType:$result);

  let hasFolder = 1;
}

//===----------------------------------------------------------------------===//
// CallOp
//===----------------------------------------------------------------------===//

def AIR_CallOp : AIR_Op<"call", []> {
  let summary = "AIR function call operation";
  let description = [{
    Calls a function in AIR.
  }];

  let arguments = (ins
    FlatSymbolRefAttr:$callee,
    Variadic<AIR_AnyType>:$operands
  );

  let results = (outs Optional<AIR_AnyType>:$result);

  let assemblyFormat = "$callee `(` $operands `)` attr-dict `:` functional-type($operands, results)";
}

//===----------------------------------------------------------------------===//
// ReturnOp
//===----------------------------------------------------------------------===//

def AIR_ReturnOp : AIR_Op<"return", [
    Pure,
    Terminator
  ]> {
  let summary = "AIR return operation";
  let description = [{
    Returns from an AIR function.
  }];

  let arguments = (ins Optional<AIR_AnyType>:$value);

  let assemblyFormat = "($value^ `:` type($value))? attr-dict";
}

#endif // AIR_OPS_TD
