#ifndef AIR_OPS_H
#define AIR_OPS_H

#include "compiler/airc-codegen/include/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/AIR/AIRTypes.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Dialect.h"
#include "mlir/IR/OpDefinition.h"
#include "mlir/Interfaces/CallInterfaces.h"
#include "mlir/Interfaces/FunctionInterfaces.h"
#include "mlir/Interfaces/SideEffectInterfaces.h"

#define GET_OP_CLASSES
#include "compiler/airc-codegen/lib/AIR/AIROps.h.inc"

#endif // AIR_OPS_H
