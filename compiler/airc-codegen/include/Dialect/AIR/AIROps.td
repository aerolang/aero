#ifndef AIR_OPS_TD
#define AIR_OPS_TD

include "compiler/airc-codegen/include/Dialect/AIR/AIRDialect.td"
include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.td"
include "mlir/IR/OpBase.td"
include "mlir/IR/EnumAttr.td"
include "mlir/Interfaces/FunctionInterfaces.td"
include "mlir/Interfaces/SideEffectInterfaces.td"

// Base class for AIR operations
class AIR_Op<string mnemonic, list<Trait> traits = []> :
    Op<AIR_Dialect, mnemonic, traits>;

class AIR_BinIntOp<string mnemonic> : AIR_Op<mnemonic, [Pure]> {
  let arguments = (ins AIR_IntType:$lhs, AIR_IntType:$rhs);
  let results = (outs AIR_IntType:$result);
  let assemblyFormat = "$lhs `,` $rhs attr-dict `:` type($result)";
}

def AIR_AddOp : AIR_BinIntOp<"add"> { let summary = "integer addition"; }
def AIR_SubOp : AIR_BinIntOp<"sub"> { let summary = "integer subtraction"; }
def AIR_MulOp : AIR_BinIntOp<"mul"> { let summary = "integer multiplication"; }
def AIR_DivOp : AIR_BinIntOp<"div"> { let summary = "integer division"; }

def AIR_ICmpPredicate : I64EnumAttr<"ICmpPredicate",
    "integer comparison predicate", [
      I64EnumAttrCase<"eq",  0>,
      I64EnumAttrCase<"ne",  1>,
      I64EnumAttrCase<"lt",  2>,
      I64EnumAttrCase<"le",  3>,
      I64EnumAttrCase<"gt",  4>,
      I64EnumAttrCase<"ge",  5>
    ]> {
  let cppNamespace = "::mlir::air";
}

def AIR_ICmpOp : AIR_Op<"icmp", [Pure]> {
  let summary = "integer comparison";
  let arguments = (ins AIR_ICmpPredicate:$predicate, AIR_IntType:$lhs, AIR_IntType:$rhs);
  let results = (outs AIR_BoolType:$result);
  let assemblyFormat = "$predicate `(` $lhs `,` $rhs `)` attr-dict `:` `(` type($lhs) `,` type($rhs) `)` `->` type($result)";
}

def AIR_FuncDefOp : AIR_Op<"func_def", [
    IsolatedFromAbove,
    FunctionOpInterface
  ]> {
  let summary = "AIR function definition";
  let description = [{
    Defines a named function in AIR.
  }];

  let arguments = (ins
    SymbolNameAttr:$sym_name,
    TypeAttrOf<FunctionType>:$function_type,
    OptionalAttr<DictArrayAttr>:$arg_attrs,
    OptionalAttr<DictArrayAttr>:$res_attrs
  );

  let regions = (region AnyRegion:$body);

  let hasCustomAssemblyFormat = 1;

  let extraClassDeclaration = [{
    ArrayRef<Type> getArgumentTypes() { return getFunctionType().getInputs(); }
    ArrayRef<Type> getResultTypes() { return getFunctionType().getResults(); }
    Region *getCallableRegion() { return &getBody(); }
  }];
}

def AIR_MainDefOp : AIR_Op<"main_def", [IsolatedFromAbove]> {
  let summary = "AIR main entry point definition";
  let description = [{
    Defines the program entry point. Takes no arguments and returns void.
  }];

  let regions = (region AnyRegion:$body);

  let assemblyFormat = "$body attr-dict";
}

def AIR_ConstantOp : AIR_Op<"constant", [Pure, ConstantLike]> {
  let summary = "AIR constant operation";
  let description = [{
    Produces a constant value.
  }];

  let arguments = (ins AnyAttr:$value);
  let results = (outs AIR_AnyType:$result);

  let hasCustomAssemblyFormat = 1;

  let hasFolder = 1;
  let hasVerifier = 1;
}

def AIR_CallOp : AIR_Op<"call", [MemoryEffects<[MemWrite]>]> {
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

def AIR_FuncRefOp : AIR_Op<"func_ref", [Pure]> {
  let summary = "produce a reference to a named AIR function";
  let arguments = (ins FlatSymbolRefAttr:$callee);
  let results = (outs AIR_FuncType:$result);
  let hasCustomAssemblyFormat = 1;
}

def AIR_IndirectCallOp : AIR_Op<"indirect_call", [MemoryEffects<[MemWrite]>]> {
  let summary = "call a function through an !air.func value";
  let arguments = (ins AIR_FuncType:$callee, Variadic<AIR_AnyType>:$args);
  let results = (outs Optional<AIR_AnyType>:$result);
  let hasCustomAssemblyFormat = 1;
}

def AIR_YieldOp : AIR_Op<"yield", [Terminator]> {
  let summary = "AIR yield - terminates a branch of air.if";
  let description = [{
    Yields a value from one branch of an air.if region.
  }];

  let arguments = (ins Optional<AnyType>:$value);

  let assemblyFormat = "($value^ `:` type($value))? attr-dict";
}

def AIR_IfOp : AIR_Op<"if", []> {
  let summary = "AIR conditional expression";
  let description = [{
    Evaluates the condition and executes exactly one of the two branch regions.
    The last value produced in the taken region is the result of the op.
    Both branches must produce a value of the same type (or both void).
  }];

  let arguments = (ins AIR_BoolType:$condition);
  let results   = (outs Optional<AnyType>:$result);
  let regions   = (region AnyRegion:$thenRegion, AnyRegion:$elseRegion);

  let hasCustomAssemblyFormat = 1;
  let hasVerifier = 1;
}

def AIR_ReturnOp : AIR_Op<"return", [Terminator]> {
  let summary = "AIR return operation";
  let description = [{
    Returns from an AIR function.
  }];

  let arguments = (ins Optional<AnyType>:$value);

  let assemblyFormat = "($value^ `:` type($value))? attr-dict";
}

def AIR_StructDefOp : AIR_Op<"struct_def", [Symbol]> {
  let summary = "AIR named struct type declaration";
  let description = [{
    Declares a named struct type at module scope.
    Example: air.struct_def @point(!air.int, !air.int)
  }];
  let arguments = (ins SymbolNameAttr:$sym_name, TypeArrayAttr:$field_types);
  let hasCustomAssemblyFormat = 1;
}

def AIR_ConstructOp : AIR_Op<"construct", [Pure]> {
  let summary = "AIR struct construction";
  let description = [{
    Constructs a named struct value from its positional field values.
    Example: air.construct @point(%x, %y) : (!air.int, !air.int) -> !air.struct<!air.int, !air.int>
  }];
  let arguments = (ins FlatSymbolRefAttr:$struct_name, Variadic<AIR_AnyType>:$fields);
  let results = (outs AIR_StructType:$result);
  let hasCustomAssemblyFormat = 1;
  let hasVerifier = 1;
}

def AIR_FieldOp : AIR_Op<"field", [Pure]> {
  let summary = "AIR struct field extraction";
  let description = [{
    Extracts a field from a struct value by 0-based index.
    Example: air.field $p[0] : (!air.struct<!air.int, !air.int>) -> !air.int
  }];
  let arguments = (ins AIR_StructType:$input, I64Attr:$index);
  let results = (outs AIR_AnyType:$result);
  let hasCustomAssemblyFormat = 1;
  let hasVerifier = 1;
}

#endif // AIR_OPS_TD
