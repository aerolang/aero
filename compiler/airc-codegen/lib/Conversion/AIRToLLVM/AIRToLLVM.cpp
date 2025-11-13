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
      // Str -> struct { ptr, i64 }
      auto ptrType = LLVM::LLVMPointerType::get(ctx);
      auto i64Type = IntegerType::get(ctx, 64);
      return LLVM::LLVMStructType::getLiteral(ctx, {ptrType, i64Type});
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

    // Rename main to aero_main - we'll create the real main wrapper
    StringRef funcName = op.getSymName();
    if (funcName == "main") {
      funcName = "aero_main";
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

      // Create a global string constant (without null terminator)
      auto moduleOp = op->getParentOfType<ModuleOp>();
      OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(moduleOp.getBody());

      std::string globalName = "str_" + std::to_string(reinterpret_cast<uintptr_t>(op.getOperation()));
      size_t strLen = stringAttr.getValue().size();

      rewriter.create<LLVM::GlobalOp>(
          op.getLoc(),
          LLVM::LLVMArrayType::get(IntegerType::get(op.getContext(), 8), strLen),
          /*isConstant=*/true,
          LLVM::Linkage::Private,
          globalName,
          rewriter.getStringAttr(stringAttr.getValue().str()));

      // Create struct { ptr, i64 } with pointer to the string data and its length
      rewriter.setInsertionPoint(op);
      auto addressOf = rewriter.create<LLVM::AddressOfOp>(
          op.getLoc(),
          LLVM::LLVMPointerType::get(op.getContext()),
          globalName);

      auto ptrType = LLVM::LLVMPointerType::get(op.getContext());
      auto i64Type = IntegerType::get(op.getContext(), 64);
      auto structType = LLVM::LLVMStructType::getLiteral(op.getContext(), {ptrType, i64Type});

      // Create an undef struct and insert the pointer and length
      auto undefStruct = rewriter.create<LLVM::UndefOp>(op.getLoc(), structType);
      auto structWithPtr = rewriter.create<LLVM::InsertValueOp>(
          op.getLoc(), undefStruct, addressOf, 0);
      auto lenConst = rewriter.create<LLVM::ConstantOp>(
          op.getLoc(), i64Type, rewriter.getI64IntegerAttr(strLen));
      auto finalStruct = rewriter.create<LLVM::InsertValueOp>(
          op.getLoc(), structWithPtr, lenConst, 1);

      rewriter.replaceOp(op, finalStruct.getResult());
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

    // Declare aero_log function if not already declared
    LLVM::LLVMFuncOp logFuncOp;
    if (!(logFuncOp = moduleOp.lookupSymbol<LLVM::LLVMFuncOp>("aero_log"))) {
      OpBuilder::InsertionGuard guard(rewriter);
      rewriter.setInsertionPointToStart(moduleOp.getBody());

      // aero_log takes a struct { ptr, i64 }
      auto ptrType = LLVM::LLVMPointerType::get(op.getContext());
      auto i64Type = IntegerType::get(op.getContext(), 64);
      auto structType = LLVM::LLVMStructType::getLiteral(op.getContext(), {ptrType, i64Type});
      auto voidType = LLVM::LLVMVoidType::get(op.getContext());
      auto funcType = LLVM::LLVMFunctionType::get(voidType, {structType});

      logFuncOp = rewriter.create<LLVM::LLVMFuncOp>(
          op.getLoc(), "aero_log", funcType);
    }

    // Create call to aero_log
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

    // After conversion, create a aero$entrypoint function that wraps aero_main
    OpBuilder builder(context);
    builder.setInsertionPointToEnd(module.getBody());

    // Check if aero_main exists
    auto aeroMainFunc = module.lookupSymbol<LLVM::LLVMFuncOp>("aero_main");
    if (aeroMainFunc) {
      // Create aero$entrypoint function: i32 @aero$entrypoint()
      auto i32Type = IntegerType::get(context, 32);
      auto entrypointFuncType = LLVM::LLVMFunctionType::get(i32Type, {});
      auto entrypointFunc = builder.create<LLVM::LLVMFuncOp>(
          module.getLoc(), "aero$entrypoint", entrypointFuncType);

      // Create the function body
      auto *entryBlock = entrypointFunc.addEntryBlock(builder);
      builder.setInsertionPointToStart(entryBlock);

      // Call aero_main()
      builder.create<LLVM::CallOp>(module.getLoc(), aeroMainFunc, ValueRange{});

      // Return 0
      auto zero = builder.create<LLVM::ConstantOp>(
          module.getLoc(), i32Type, builder.getI32IntegerAttr(0));
      builder.create<LLVM::ReturnOp>(module.getLoc(), ValueRange{zero});
    }
  }
};

} // namespace

std::unique_ptr<Pass> mlir::air::createConvertAIRToLLVMPass() {
  return std::make_unique<ConvertAIRToLLVMPass>();
}
