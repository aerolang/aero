#include "compiler/airc-codegen/include/Dialect/AIR/AIRToLLVM.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.h"
#include "compiler/airc-codegen/lib/Utils/AIRTypeConverter.h"

#include "mlir/Conversion/LLVMCommon/ConversionTarget.h"
#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/PatternMatch.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Transforms/DialectConversion.h"

namespace mlir::air {
#define GEN_PASS_DEF_CONVERTAIRTOLLVM
#include "compiler/airc-codegen/lib/Conversion/AIRToLLVM.h.inc"
} // namespace mlir::air

using namespace mlir;
using namespace mlir::air;

namespace {

//===----------------------------------------------------------------------===//
// Pattern Conversions
//===----------------------------------------------------------------------===//

struct ConstantOpConversion : public OpConversionPattern<air::ConstantOp> {
  ConstantOpConversion(TypeConverter &typeConverter, MLIRContext *ctx,
                       unsigned &strCounter)
      : OpConversionPattern(typeConverter, ctx), strCounter(strCounter) {}

  unsigned &strCounter;

  LogicalResult
  matchAndRewrite(air::ConstantOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    Type resultType = op.getType();

    if (isa<StrType>(resultType)) {
      auto stringAttr = dyn_cast<StringAttr>(op.getValue());
      if (!stringAttr)
        return failure();

      auto moduleOp = op->getParentOfType<ModuleOp>();
      OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(moduleOp.getBody());

      std::string globalName = "str_global_" + std::to_string(strCounter++);
      size_t strLen = stringAttr.getValue().size();

      LLVM::GlobalOp::create(
          rewriter, op.getLoc(),
          LLVM::LLVMArrayType::get(IntegerType::get(op.getContext(), 8),
                                   strLen),
          /*isConstant=*/true, LLVM::Linkage::Private, globalName,
          rewriter.getStringAttr(stringAttr.getValue().str()));

      rewriter.setInsertionPoint(op);
      auto addressOf = LLVM::AddressOfOp::create(
          rewriter, op.getLoc(), LLVM::LLVMPointerType::get(op.getContext()),
          globalName);

      auto ptrType = LLVM::LLVMPointerType::get(op.getContext());
      auto i64Type = IntegerType::get(op.getContext(), 64);
      auto structType =
          LLVM::LLVMStructType::getLiteral(op.getContext(), {ptrType, i64Type});

      auto undefStruct =
          LLVM::UndefOp::create(rewriter, op.getLoc(), structType);
      auto structWithPtr =
          LLVM::InsertValueOp::create(rewriter, op.getLoc(), undefStruct,
                                      addressOf, llvm::ArrayRef<int64_t>{0});
      auto lenConst = LLVM::ConstantOp::create(
          rewriter, op.getLoc(), i64Type, rewriter.getI64IntegerAttr(strLen));
      auto finalStruct =
          LLVM::InsertValueOp::create(rewriter, op.getLoc(), structWithPtr,
                                      lenConst, llvm::ArrayRef<int64_t>{1});

      rewriter.replaceOp(op, finalStruct.getResult());
      return success();
    }

    if (isa<IntType>(resultType)) {
      auto intAttr = dyn_cast<IntegerAttr>(op.getValue());
      if (!intAttr)
        return failure();
      rewriter.replaceOpWithNewOp<LLVM::ConstantOp>(
          op, IntegerType::get(op.getContext(), 64), intAttr);
      return success();
    }

    if (isa<BoolType>(resultType)) {
      auto intAttr = dyn_cast<IntegerAttr>(op.getValue());
      if (!intAttr)
        return failure();
      rewriter.replaceOpWithNewOp<LLVM::ConstantOp>(
          op, IntegerType::get(op.getContext(), 1), intAttr);
      return success();
    }

    if (isa<VoidType>(resultType)) {
      rewriter.eraseOp(op);
      return success();
    }

    return failure();
  }
};

struct IndirectCallOpConversion
    : public OpConversionPattern<air::IndirectCallOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::IndirectCallOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    const TypeConverter *tc = getTypeConverter();
    auto airFuncType = mlir::dyn_cast<air::FuncType>(op.getCallee().getType());
    if (!airFuncType)
      return rewriter.notifyMatchFailure(op, "expected !air.func callee type");

    SmallVector<Type> argTypes;
    if (failed(tc->convertTypes(airFuncType.getInputs(), argTypes)))
      return failure();
    SmallVector<Type> resultTypes;
    for (Type t : airFuncType.getOutputs()) {
      if (isa<air::VoidType>(t))
        continue;
      auto c = tc->convertType(t);
      if (!c)
        return failure();
      resultTypes.push_back(c);
    }
    // adaptor.getCallee() is already a ptr (converted from !air.func).
    // Build operands with callee ptr first, then args (LLVM indirect call
    // convention).
    SmallVector<Value> calleeAndArgs;
    calleeAndArgs.push_back(adaptor.getCallee());
    calleeAndArgs.append(adaptor.getArgs().begin(), adaptor.getArgs().end());

    auto callOp = LLVM::CallOp::create(
        rewriter, op.getLoc(),
        resultTypes.empty() ? TypeRange{} : TypeRange{resultTypes},
        calleeAndArgs);
    // operandSegmentSizes: [callee_operands=N, op_bundle_operands=0]
    // For indirect call: callee ptr + args = calleeAndArgs.size().
    callOp.getProperties().operandSegmentSizes = {
        static_cast<int32_t>(calleeAndArgs.size()), 0};
    callOp.getProperties().op_bundle_sizes = rewriter.getDenseI32ArrayAttr({});
    if (op.getNumResults() == 0)
      rewriter.eraseOp(op);
    else
      rewriter.replaceOp(op, callOp.getResults());
    return success();
  }
};

struct StructDefOpConversion : public OpConversionPattern<air::StructDefOp> {
  using OpConversionPattern::OpConversionPattern;
  LogicalResult
  matchAndRewrite(air::StructDefOp op, OpAdaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.eraseOp(op);
    return success();
  }
};

struct ConstructOpConversion : public OpConversionPattern<air::ConstructOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::ConstructOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    auto *typeConverter = getTypeConverter();
    auto llvmStructTy = typeConverter->convertType(op.getResult().getType());
    if (!llvmStructTy)
      return failure();

    Value agg = LLVM::UndefOp::create(rewriter, op.getLoc(), llvmStructTy);
    for (auto [idx, field] : llvm::enumerate(adaptor.getFields()))
      agg = LLVM::InsertValueOp::create(rewriter, op.getLoc(), agg, field,
                                        ArrayRef<int64_t>{(int64_t)idx});
    rewriter.replaceOp(op, agg);
    return success();
  }
};

struct FieldOpConversion : public OpConversionPattern<air::FieldOp> {
  using OpConversionPattern::OpConversionPattern;

  LogicalResult
  matchAndRewrite(air::FieldOp op, OpAdaptor adaptor,
                  ConversionPatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<LLVM::ExtractValueOp>(
        op, adaptor.getInput(), ArrayRef<int64_t>{(int64_t)op.getIndex()});
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

    unsigned strCounter = 0;
    patterns.add<ConstantOpConversion>(typeConverter, context, strCounter);
    patterns.add<IndirectCallOpConversion>(typeConverter, context);
    patterns
        .add<StructDefOpConversion, ConstructOpConversion, FieldOpConversion>(
            typeConverter, context);

    LLVMConversionTarget target(*context);
    target.addLegalDialect<LLVM::LLVMDialect>();
    target.addLegalDialect<arith::ArithDialect>();
    target.addLegalDialect<func::FuncDialect>();
    target.addIllegalOp<air::ConstantOp, air::IndirectCallOp, air::StructDefOp,
                        air::ConstructOp, air::FieldOp>();
    target.markUnknownOpDynamicallyLegal([](Operation *) { return true; });

    if (failed(applyPartialConversion(module, target, std::move(patterns))))
      signalPassFailure();
  }
};

} // namespace

std::unique_ptr<Pass> mlir::air::createConvertAIRToLLVMPass() {
  return std::make_unique<ConvertAIRToLLVMPass>();
}
