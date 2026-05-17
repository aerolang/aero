#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRToLLVM.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.h"
#include "compiler/airc-codegen/lib/Utils/AIRTypeConverter.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/PatternMatch.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Transforms/DialectConversion.h"

namespace mlir::air {
#define GEN_PASS_DEF_CONVERTAIRTOFUNC
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM.h.inc"
} // namespace mlir::air

using namespace mlir;
using namespace mlir::air;

namespace {

//===----------------------------------------------------------------------===//
// Pattern Conversions
//===----------------------------------------------------------------------===//

struct FuncDefOpToFuncConversion : public OpConversionPattern<air::FuncDefOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::FuncDefOp op, OpAdaptor,
                  ConversionPatternRewriter &rewriter) const override {
    const TypeConverter *tc = getTypeConverter();
    FunctionType airFuncType = op.getFunctionType();

    SmallVector<Type> argTypes;
    if (failed(tc->convertTypes(airFuncType.getInputs(), argTypes)))
      return failure();
    // Skip void results; func.func uses empty results for void functions.
    SmallVector<Type> resultTypes;
    for (Type t : airFuncType.getResults()) {
      if (isa<air::VoidType>(t))
        continue;
      auto converted = tc->convertType(t);
      if (!converted)
        return failure();
      resultTypes.push_back(converted);
    }

    auto funcType = rewriter.getFunctionType(argTypes, resultTypes);
    auto funcOp =
        func::FuncOp::create(rewriter, op.getLoc(), op.getSymName(), funcType);

    rewriter.inlineRegionBefore(op.getBody(), funcOp.getBody(), funcOp.end());
    if (failed(rewriter.convertRegionTypes(&funcOp.getBody(), *tc)))
      return failure();

    rewriter.eraseOp(op);
    return success();
  }
};

struct MainDefOpToFuncConversion : public OpConversionPattern<air::MainDefOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::MainDefOp op, OpAdaptor,
                  ConversionPatternRewriter &rewriter) const override {
    // The OS entry point returns i32; we return 0.
    auto i32Type = IntegerType::get(op.getContext(), 32);
    auto funcType = rewriter.getFunctionType({}, {i32Type});
    auto funcOp = func::FuncOp::create(rewriter, op.getLoc(), "aero$entrypoint",
                                       funcType);
    funcOp->setAttr("is_entrypoint", rewriter.getUnitAttr());

    rewriter.inlineRegionBefore(op.getBody(), funcOp.getBody(), funcOp.end());
    if (failed(rewriter.convertRegionTypes(&funcOp.getBody(),
                                           *getTypeConverter())))
      return failure();

    rewriter.eraseOp(op);
    return success();
  }
};

// Returns or creates a func.func declaration for a runtime external.
static func::FuncOp
getOrInsertRuntimeDecl(StringRef name, FunctionType funcType, ModuleOp module,
                       ConversionPatternRewriter &rewriter) {
  if (auto existing = module.lookupSymbol<func::FuncOp>(name))
    return existing;

  OpBuilder::InsertionGuard guard(rewriter);
  rewriter.setInsertionPointToStart(module.getBody());
  auto decl =
      func::FuncOp::create(rewriter, rewriter.getUnknownLoc(), name, funcType);
  decl.setPrivate();
  return decl;
}

struct CallOpToFuncConversion : public OpConversionPattern<air::CallOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::CallOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    const TypeConverter *tc = getTypeConverter();
    auto moduleOp = op->getParentOfType<ModuleOp>();
    StringRef calleeName = op.getCallee();

    // Determine result types.
    SmallVector<Type> resultTypes;
    if (failed(tc->convertTypes(op.getResultTypes(), resultTypes)))
      return failure();

    // Check if this is a user-defined func (already lowered to func.func).
    if (moduleOp.lookupSymbol<func::FuncOp>(calleeName) ||
        moduleOp.lookupSymbol<air::FuncDefOp>(calleeName)) {
      rewriter.replaceOpWithNewOp<func::CallOp>(op, resultTypes, calleeName,
                                                adaptor.getOperands());
      return success();
    }

    // Runtime externals: build the func type from converted operand types.
    SmallVector<Type> argTypes;
    for (Type t : adaptor.getOperands().getTypes())
      argTypes.push_back(t);

    auto strType = tc->convertType(air::StrType::get(op.getContext()));

    // Map runtime symbol names to their function types.
    FunctionType runtimeType;
    if (calleeName == "runtime$log") {
      runtimeType = rewriter.getFunctionType({strType}, {});
    } else if (calleeName == "builtins$str-concat") {
      runtimeType = rewriter.getFunctionType({strType, strType}, {strType});
    } else if (calleeName == "builtins$str-free") {
      runtimeType = rewriter.getFunctionType({strType}, {});
    } else if (calleeName == "builtins$int-as-str") {
      auto i64Type = IntegerType::get(op.getContext(), 64);
      runtimeType = rewriter.getFunctionType({i64Type}, {strType});
    } else if (calleeName == "builtins$bool-as-str") {
      auto i1Type = IntegerType::get(op.getContext(), 1);
      runtimeType = rewriter.getFunctionType({i1Type}, {strType});
    } else {
      return rewriter.notifyMatchFailure(op, "unknown callee");
    }

    getOrInsertRuntimeDecl(calleeName, runtimeType, moduleOp, rewriter);
    rewriter.replaceOpWithNewOp<func::CallOp>(op, resultTypes, calleeName,
                                              adaptor.getOperands());
    return success();
  }
};

struct FuncRefOpConversion : public OpConversionPattern<air::FuncRefOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::FuncRefOp op, OpAdaptor,
                  ConversionPatternRewriter &rewriter) const override {
    const TypeConverter *tc = getTypeConverter();
    auto airFuncType = mlir::dyn_cast<air::FuncType>(op.getResult().getType());
    if (!airFuncType)
      return rewriter.notifyMatchFailure(op, "expected !air.func result type");

    // Build the lowered FunctionType: convert AIR types, strip void results.
    SmallVector<Type> argTypes;
    if (failed(tc->convertTypes(airFuncType.getInputs(), argTypes)))
      return failure();
    SmallVector<Type> resultTypes;
    for (Type t : airFuncType.getOutputs()) {
      if (isa<air::VoidType>(t))
        continue;
      auto converted = tc->convertType(t);
      if (!converted)
        return failure();
      resultTypes.push_back(converted);
    }
    auto loweredFt = rewriter.getFunctionType(argTypes, resultTypes);
    rewriter.replaceOpWithNewOp<func::ConstantOp>(op, loweredFt,
                                                  op.getCallee());
    return success();
  }
};

struct ReturnOpToFuncConversion : public OpConversionPattern<air::ReturnOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::ReturnOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    auto parentFunc = op->getParentOfType<func::FuncOp>();
    if (parentFunc && parentFunc->hasAttr("is_entrypoint")) {
      auto i32Type = IntegerType::get(op.getContext(), 32);
      auto zero = arith::ConstantOp::create(rewriter, op.getLoc(), i32Type,
                                            rewriter.getI32IntegerAttr(0));
      rewriter.replaceOpWithNewOp<func::ReturnOp>(op, ValueRange{zero});
    } else {
      // Strip void-typed return values; func.return uses an empty list.
      SmallVector<Value> retVals;
      for (auto [orig, conv] :
           llvm::zip(op.getOperands(), adaptor.getOperands()))
        if (!isa<air::VoidType>(orig.getType()))
          retVals.push_back(conv);
      rewriter.replaceOpWithNewOp<func::ReturnOp>(op, retVals);
    }
    return success();
  }
};

//===----------------------------------------------------------------------===//
// Pass Definition
//===----------------------------------------------------------------------===//

struct ConvertAIRToFuncPass
    : public ::mlir::air::impl::ConvertAIRToFuncBase<ConvertAIRToFuncPass> {
  void runOnOperation() override {
    MLIRContext *context = &getContext();
    ModuleOp module = dyn_cast<ModuleOp>(getOperation());
    if (!module)
      return signalPassFailure();

    AIRTypeConverter typeConverter(context);
    RewritePatternSet patterns(context);

    patterns.add<FuncDefOpToFuncConversion, MainDefOpToFuncConversion>(
        typeConverter, context);
    patterns.add<CallOpToFuncConversion, ReturnOpToFuncConversion>(
        typeConverter, context);
    patterns.add<FuncRefOpConversion>(typeConverter, context);

    ConversionTarget target(*context);
    target.addLegalDialect<func::FuncDialect>();
    target.addLegalDialect<arith::ArithDialect>();
    // Keep all other AIR ops (constants, arithmetic, structs, if) legal;
    // they're lowered by ConvertAIRToLLVM.
    target.addIllegalOp<air::FuncDefOp, air::MainDefOp, air::CallOp,
                        air::ReturnOp, air::FuncRefOp>();
    target.markUnknownOpDynamicallyLegal([](Operation *) { return true; });

    if (failed(applyPartialConversion(module, target, std::move(patterns))))
      signalPassFailure();
  }
};

} // namespace

std::unique_ptr<Pass> mlir::air::createConvertAIRToFuncPass() {
  return std::make_unique<ConvertAIRToFuncPass>();
}
