#ifndef AIR_TO_LLVM_H
#define AIR_TO_LLVM_H

#include "mlir/Pass/Pass.h"
#include <memory>

namespace mlir::air {

#define GEN_PASS_DECL
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM.h.inc"

std::unique_ptr<Pass> createConvertAIRToArithPass();
std::unique_ptr<Pass> createConvertAIRToFuncPass();
std::unique_ptr<Pass> createConvertAIRToLLVMPass();
std::unique_ptr<Pass> createConvertAIRToSCFPass();

#define GEN_PASS_REGISTRATION
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM.h.inc"

} // namespace mlir::air

#endif // AIR_TO_LLVM_H
