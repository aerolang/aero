#include "compiler/airc-codegen/include/AIR/AIROps.h"
#include "compiler/airc-codegen/include/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/AIR/AIRTypes.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/OpImplementation.h"

using namespace mlir;
using namespace mlir::air;

#define GET_OP_CLASSES
#include "compiler/airc-codegen/lib/AIR/AIROps.cpp.inc"

// Fold method for ConstantOp - required by ConstantLike trait
OpFoldResult ConstantOp::fold(FoldAdaptor adaptor) {
  // For constant operations, just return the constant value attribute
  return getValueAttr();
}
