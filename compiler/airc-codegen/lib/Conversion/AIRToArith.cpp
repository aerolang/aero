#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRToLLVM.h"
#include "compiler/airc-codegen/lib/Utils/AIRTypeConverter.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/PatternMatch.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Transforms/DialectConversion.h"

namespace mlir::air {
#define GEN_PASS_DEF_CONVERTAIRTOARITH
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM.h.inc"
} // namespace mlir::air

using namespace mlir;
using namespace mlir::air;

namespace {

//===----------------------------------------------------------------------===//
// Pattern Conversions
//===----------------------------------------------------------------------===//

struct AddOpConversion : public OpConversionPattern<air::AddOp> {
  using OpConversionPattern::OpConversionPattern;
  LogicalResult
  matchAndRewrite(air::AddOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<arith::AddIOp>(op, adaptor.getLhs(),
                                               adaptor.getRhs());
    return success();
  }
};

struct SubOpConversion : public OpConversionPattern<air::SubOp> {
  using OpConversionPattern::OpConversionPattern;
  LogicalResult
  matchAndRewrite(air::SubOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<arith::SubIOp>(op, adaptor.getLhs(),
                                               adaptor.getRhs());
    return success();
  }
};

struct MulOpConversion : public OpConversionPattern<air::MulOp> {
  using OpConversionPattern::OpConversionPattern;
  LogicalResult
  matchAndRewrite(air::MulOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<arith::MulIOp>(op, adaptor.getLhs(),
                                               adaptor.getRhs());
    return success();
  }
};

struct DivOpConversion : public OpConversionPattern<air::DivOp> {
  using OpConversionPattern::OpConversionPattern;
  LogicalResult
  matchAndRewrite(air::DivOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<arith::DivSIOp>(op, adaptor.getLhs(),
                                                adaptor.getRhs());
    return success();
  }
};

struct ICmpOpConversion : public OpConversionPattern<air::ICmpOp> {
  using OpConversionPattern::OpConversionPattern;
  LogicalResult
  matchAndRewrite(air::ICmpOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    arith::CmpIPredicate pred;
    switch (op.getPredicate()) {
    case ICmpPredicate::eq:
      pred = arith::CmpIPredicate::eq;
      break;
    case ICmpPredicate::ne:
      pred = arith::CmpIPredicate::ne;
      break;
    case ICmpPredicate::lt:
      pred = arith::CmpIPredicate::slt;
      break;
    case ICmpPredicate::le:
      pred = arith::CmpIPredicate::sle;
      break;
    case ICmpPredicate::gt:
      pred = arith::CmpIPredicate::sgt;
      break;
    case ICmpPredicate::ge:
      pred = arith::CmpIPredicate::sge;
      break;
    default:
      return rewriter.notifyMatchFailure(op, "unknown ICmp predicate");
    }
    rewriter.replaceOpWithNewOp<arith::CmpIOp>(op, pred, adaptor.getLhs(),
                                               adaptor.getRhs());
    return success();
  }
};

//===----------------------------------------------------------------------===//
// Pass Definition
//===----------------------------------------------------------------------===//

struct ConvertAIRToArithPass
    : public ::mlir::air::impl::ConvertAIRToArithBase<ConvertAIRToArithPass> {
  void runOnOperation() override {
    MLIRContext *context = &getContext();
    ModuleOp module = dyn_cast<ModuleOp>(getOperation());
    if (!module)
      return signalPassFailure();

    AIRTypeConverter typeConverter(context);
    RewritePatternSet patterns(context);
    patterns.add<AddOpConversion, SubOpConversion, MulOpConversion,
                 DivOpConversion, ICmpOpConversion>(typeConverter, context);

    ConversionTarget target(*context);
    target.addLegalDialect<arith::ArithDialect>();
    target.addIllegalOp<air::AddOp, air::SubOp, air::MulOp, air::DivOp,
                        air::ICmpOp>();
    target.markUnknownOpDynamicallyLegal([](Operation *) { return true; });

    if (failed(applyPartialConversion(module, target, std::move(patterns))))
      signalPassFailure();
  }
};

} // namespace

std::unique_ptr<Pass> mlir::air::createConvertAIRToArithPass() {
  return std::make_unique<ConvertAIRToArithPass>();
}
