#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/lib/Utils/AIRTypeConverter.h"

#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/PatternMatch.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Transforms/DialectConversion.h"

namespace mlir::air {
#define GEN_PASS_DEF_CONVERTAIRTOSCF
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM.h.inc"
} // namespace mlir::air

using namespace mlir;
using namespace mlir::air;

namespace {

//===----------------------------------------------------------------------===//
// Pattern Conversions
//===----------------------------------------------------------------------===//

struct IfOpToSCFConversion : public OpConversionPattern<air::IfOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::IfOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    SmallVector<Type> resultTypes;
    if (op.getResult()) {
      if (failed(getTypeConverter()->convertType(op.getResult().getType(),
                                                 resultTypes)))
        return failure();
    }

    auto scfIf = scf::IfOp::create(rewriter, op.getLoc(), resultTypes,
                                   adaptor.getCondition(),
                                   /*withElseRegion=*/true);

    auto replaceRegion = [&](Region &src, Region &dst) {
      rewriter.inlineRegionBefore(src, dst, dst.begin());
      Block &emptyBlock = dst.back();
      assert(emptyBlock.empty() && "expected trailing empty block");
      rewriter.eraseBlock(&emptyBlock);
    };
    replaceRegion(op.getThenRegion(), scfIf.getThenRegion());
    replaceRegion(op.getElseRegion(), scfIf.getElseRegion());

    rewriter.replaceOp(op, scfIf.getResults());
    return success();
  }
};

struct YieldOpToSCFConversion : public OpConversionPattern<air::YieldOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::YieldOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<scf::YieldOp>(op, adaptor.getOperands());
    return success();
  }
};

//===----------------------------------------------------------------------===//
// Pass Definition
//===----------------------------------------------------------------------===//

struct ConvertAIRToSCFPass
    : public ::mlir::air::impl::ConvertAIRToSCFBase<ConvertAIRToSCFPass> {
  void runOnOperation() override {
    MLIRContext *context = &getContext();
    ModuleOp module = dyn_cast<ModuleOp>(getOperation());
    if (!module)
      return signalPassFailure();

    AIRTypeConverter typeConverter(context);
    RewritePatternSet patterns(context);
    patterns.add<IfOpToSCFConversion, YieldOpToSCFConversion>(typeConverter,
                                                              context);

    ConversionTarget target(*context);
    target.addLegalDialect<scf::SCFDialect>();
    target.addIllegalOp<air::IfOp, air::YieldOp>();
    target.markUnknownOpDynamicallyLegal([](Operation *) { return true; });

    if (failed(applyPartialConversion(module, target, std::move(patterns))))
      signalPassFailure();
  }
};

} // namespace

namespace mlir::air {
std::unique_ptr<Pass> createConvertAIRToSCFPass() {
  return std::make_unique<ConvertAIRToSCFPass>();
}
} // namespace mlir::air
