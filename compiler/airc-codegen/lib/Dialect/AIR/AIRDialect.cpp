#include "compiler/airc-codegen/include/Dialect/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.h"

#include "mlir/IR/Builders.h"
#include "mlir/IR/DialectImplementation.h"
#include "llvm/ADT/TypeSwitch.h"

using namespace mlir;
using namespace mlir::air;

#include "compiler/airc-codegen/lib/Dialect/AIR/AIRDialect.cpp.inc"

// Include type definitions (with MLIR_DEFINE_EXPLICIT_TYPE_ID and parse/print
// methods)
#define GET_TYPEDEF_CLASSES
#include "compiler/airc-codegen/lib/Dialect/AIR/AIRTypes.cpp.inc"

void AIRDialect::initialize() {
  addOperations<
#define GET_OP_LIST
#include "compiler/airc-codegen/lib/Dialect/AIR/AIROps.cpp.inc"
      >();
  addTypes<
#define GET_TYPEDEF_LIST
#include "compiler/airc-codegen/lib/Dialect/AIR/AIRTypes.cpp.inc"
      >();
}

// Stub attribute parser (we have no custom attributes)
mlir::Attribute AIRDialect::parseAttribute(mlir::DialectAsmParser &parser,
                                           mlir::Type type) const {
  return {};
}

// Stub attribute printer (we have no custom attributes)
void AIRDialect::printAttribute(mlir::Attribute attr,
                                mlir::DialectAsmPrinter &printer) const {}
