#ifndef AIR_DIALECT_TD
#define AIR_DIALECT_TD

include "mlir/IR/OpBase.td"

def AIR_Dialect : Dialect {
  let name = "air";
  let summary = "AIR (Aero Intermediate Representation) dialect";
  let description = [{
    The AIR dialect represents the intermediate representation for the Aero
    programming language. It provides a simple, functional programming model
    with explicit types and basic operations.
  }];

  let cppNamespace = "::mlir::air";

  // Use default type printer/parser
  let useDefaultTypePrinterParser = 1;

  // Use default attribute printer/parser
  let useDefaultAttributePrinterParser = 1;
}

#endif // AIR_DIALECT_TD
