#ifndef AIR_TYPES_TD
#define AIR_TYPES_TD

include "compiler/airc-codegen/include/Dialect/AIR/AIRDialect.td"
include "mlir/IR/AttrTypeBase.td"

class AIR_Type<string name, string typeMnemonic> : TypeDef<AIR_Dialect, name> {
  let mnemonic = typeMnemonic;
}

def AIR_VoidType : AIR_Type<"Void", "void"> {
  let summary = "AIR void type";
  let description = [{
    Represents a void type (unit type) in AIR.
  }];
}

def AIR_BoolType : AIR_Type<"Bool", "bool"> {
  let summary = "AIR boolean type";
  let description = [{
    Represents a boolean type in AIR.
  }];
}

def AIR_IntType : AIR_Type<"Int", "int"> {
  let summary = "AIR integer type";
  let description = [{
    Represents an integer type in AIR.
  }];
}

def AIR_StrType : AIR_Type<"Str", "str"> {
  let summary = "AIR string type";
  let description = [{
    Represents a string type in AIR.
  }];
}

def AIR_StructType : AIR_Type<"Struct", "struct"> {
  let summary = "AIR struct type";
  let description = [{
    Represents a positional struct type in AIR, parameterized by its field types.
  }];
  let parameters = (ins ArrayRefParameter<"Type">:$fieldTypes);
  let assemblyFormat = "`<` $fieldTypes `>`";
}

def AIR_FuncType : AIR_Type<"Func", "func"> {
  let summary = "AIR function type";
  let description = [{
    Represents a function as a first-class value in AIR.
    Parameterized by input and output AIR types.
  }];
  let parameters = (ins ArrayRefParameter<"Type">:$inputs,
                        ArrayRefParameter<"Type">:$outputs);
  let assemblyFormat = "`<` `(` $inputs `)` `->` $outputs `>`";
}

// Type constraints for use in operations
def AIR_AnyType : AnyTypeOf<[AIR_IntType, AIR_StrType, AIR_BoolType, AIR_VoidType, AIR_StructType, AIR_FuncType]>;

#endif // AIR_TYPES_TD
