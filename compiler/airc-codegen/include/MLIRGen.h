#ifndef AIRC_MLIRGEN_H
#define AIRC_MLIRGEN_H

#include "compiler/airc-codegen/schema/air_ast_generated.h"

#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"

namespace airc::codegen {

// Constructs an AIR MLIR module from a FlatBuffers Source.
// Returns the module on success, or a null ModuleOp on failure.
// Diagnostics are emitted through the context's diagnostic engine.
mlir::OwningOpRef<mlir::ModuleOp> mlirGen(mlir::MLIRContext &context,
                                          const air_ast::Source *source,
                                          const air_ast::MainDef *mainDef,
                                          const std::string &filename);

} // namespace airc::codegen

#endif // AIRC_MLIRGEN_H
