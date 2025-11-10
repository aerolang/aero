#include "compiler/airc-codegen/include/AIR/AIRToLLVM.h"
#include "compiler/airc-codegen/include/AIR/AIRDialect.h"
#include "compiler/airc-codegen/include/AIR/AIROps.h"
#include "compiler/airc-codegen/include/AIR/AIRTypes.h"
#include "mlir/Conversion/LLVMCommon/ConversionTarget.h"
#include "mlir/Conversion/LLVMCommon/Pattern.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/PatternMatch.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Transforms/DialectConversion.h"
#include "llvm/ADT/TypeSwitch.h"

namespace mlir {
namespace air {
#define GEN_PASS_DEF_CONVERTAIRTOLLVM
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM/AIRToLLVM.h.inc"
} // namespace air
} // namespace mlir

using namespace mlir;
using namespace mlir::air;

namespace {

//===----------------------------------------------------------------------===//
// Type Conversion
//===----------------------------------------------------------------------===//

class AIRTypeConverter : public TypeConverter {
public:
  AIRTypeConverter(MLIRContext *ctx) {
    // Convert AIR types to LLVM types
    addConversion([](Type type) { return type; });
    addConversion([ctx](IntType type) {
      return IntegerType::get(ctx, 64); // Int -> i64
    });
    addConversion([ctx](StrType type) {
      return LLVM::LLVMPointerType::get(ctx); // Str -> ptr
    });
    addConversion([](VoidType type) {
      return LLVM::LLVMVoidType::get(type.getContext()); // Void -> void
    });
  }
};

//===----------------------------------------------------------------------===//
// Pattern Conversions
//===----------------------------------------------------------------------===//

struct FuncOpConversion : public OpConversionPattern<air::FuncOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::FuncOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    // Convert function type
    const TypeConverter *typeConverter = getTypeConverter();
    FunctionType funcType = op.getFunctionType();

    SmallVector<Type> argTypes;
    if (failed(typeConverter->convertTypes(funcType.getInputs(), argTypes)))
      return failure();

    SmallVector<Type> resultTypes;
    if (failed(typeConverter->convertTypes(funcType.getResults(), resultTypes)))
      return failure();

    // Rename main to air_main - the runtime will provide the real main wrapper
    StringRef funcName = op.getSymName();
    if (funcName == "main") {
      funcName = "air_main";
    }

    // LLVM function uses LLVM function type, not standard MLIR FunctionType
    auto llvmFuncType = LLVM::LLVMFunctionType::get(
        resultTypes.empty() ? LLVM::LLVMVoidType::get(op.getContext()) : resultTypes[0],
        argTypes);

    // Create LLVM function
    auto llvmFunc = rewriter.create<LLVM::LLVMFuncOp>(
        op.getLoc(), funcName, llvmFuncType);

    // Inline the function body
    rewriter.inlineRegionBefore(op.getBody(), llvmFunc.getBody(),
                                llvmFunc.end());

    // Convert block arguments
    if (failed(rewriter.convertRegionTypes(&llvmFunc.getBody(), *typeConverter)))
      return failure();

    rewriter.eraseOp(op);
    return success();
  }
};

struct ConstantOpConversion : public OpConversionPattern<air::ConstantOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::ConstantOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Type resultType = op.getType();

    // Handle string constants
    if (isa<StrType>(resultType)) {
      auto stringAttr = dyn_cast<StringAttr>(op.getValue());
      if (!stringAttr)
        return failure();

      // Create a global string constant
      auto moduleOp = op->getParentOfType<ModuleOp>();
      OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(moduleOp.getBody());

      std::string globalName = "str_" + std::to_string(reinterpret_cast<uintptr_t>(op.getOperation()));
      rewriter.create<LLVM::GlobalOp>(
          op.getLoc(),
          LLVM::LLVMArrayType::get(IntegerType::get(op.getContext(), 8),
                                   stringAttr.getValue().size() + 1),
          /*isConstant=*/true,
          LLVM::Linkage::Private,
          globalName,
          rewriter.getStringAttr(stringAttr.getValue().str() + '\0'));

      // Get address of global
      rewriter.setInsertionPoint(op);
      auto addressOf = rewriter.create<LLVM::AddressOfOp>(
          op.getLoc(),
          LLVM::LLVMPointerType::get(op.getContext()),
          globalName);

      rewriter.replaceOp(op, addressOf.getResult());
      return success();
    }

    // Handle void constants
    if (isa<VoidType>(resultType)) {
      // Void constants are just removed in LLVM
      rewriter.eraseOp(op);
      return success();
    }

    return failure();
  }
};

struct LogOpConversion : public OpConversionPattern<air::LogOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::LogOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    auto moduleOp = op->getParentOfType<ModuleOp>();

    // Declare air_log function if not already declared
    LLVM::LLVMFuncOp logFuncOp;
    if (!(logFuncOp = moduleOp.lookupSymbol<LLVM::LLVMFuncOp>("air_log"))) {
      OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(moduleOp.getBody());

      auto ptrType = LLVM::LLVMPointerType::get(op.getContext());
      auto voidType = LLVM::LLVMVoidType::get(op.getContext());
      auto funcType = LLVM::LLVMFunctionType::get(voidType, {ptrType});

      logFuncOp = rewriter.create<LLVM::LLVMFuncOp>(
          op.getLoc(), "air_log", funcType);
    }

    // Create call to air_log
    rewriter.replaceOpWithNewOp<LLVM::CallOp>(
        op, logFuncOp, adaptor.getValue());

    return success();
  }
};

struct ReturnOpConversion : public OpConversionPattern<air::ReturnOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::ReturnOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<LLVM::ReturnOp>(op, adaptor.getOperands());
    return success();
  }
};

//===----------------------------------------------------------------------===//
// Pass Definition
//===----------------------------------------------------------------------===//

struct ConvertAIRToLLVMPass
    : public ::mlir::air::impl::ConvertAIRToLLVMBase<ConvertAIRToLLVMPass> {
  void runOnOperation() override {
    MLIRContext *context = &getContext();
    ModuleOp module = dyn_cast<ModuleOp>(getOperation());
    if (!module)
      return signalPassFailure();

    AIRTypeConverter typeConverter(context);
    RewritePatternSet patterns(context);

    patterns.add<FuncOpConversion, ConstantOpConversion, LogOpConversion,
                 ReturnOpConversion>(typeConverter, context);

    LLVMConversionTarget target(*context);
    target.addLegalDialect<LLVM::LLVMDialect>();
    target.addIllegalDialect<AIRDialect>();

    if (failed(applyPartialConversion(module, target, std::move(patterns))))
      signalPassFailure();
  }
};

} // namespace

std::unique_ptr<Pass> mlir::air::createConvertAIRToLLVMPass() {
  return std::make_unique<ConvertAIRToLLVMPass>();
}
