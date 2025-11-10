#ifndef AIR_TO_LLVM_H
#define AIR_TO_LLVM_H

#include "mlir/Pass/Pass.h"
#include <memory>

namespace mlir {
namespace air {

#define GEN_PASS_DECL
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM/AIRToLLVM.h.inc"

std::unique_ptr<Pass> createConvertAIRToLLVMPass();

#define GEN_PASS_REGISTRATION
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM/AIRToLLVM.h.inc"

} // namespace air
} // namespace mlir

#endif // AIR_TO_LLVM_H
