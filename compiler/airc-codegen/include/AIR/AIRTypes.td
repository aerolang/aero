#ifndef AIR_TYPES_TD
#define AIR_TYPES_TD

include "compiler/airc-codegen/include/AIR/AIRDialect.td"
include "mlir/IR/AttrTypeBase.td"

class AIR_Type<string name, string typeMnemonic> : TypeDef<AIR_Dialect, name> {
  let mnemonic = typeMnemonic;
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

def AIR_VoidType : AIR_Type<"Void", "void"> {
  let summary = "AIR void type";
  let description = [{
    Represents a void type (unit type) in AIR.
  }];
}

// Type constraints for use in operations
def AIR_AnyType : AnyTypeOf<[AIR_IntType, AIR_StrType, AIR_VoidType]>;

#endif // AIR_TYPES_TD
