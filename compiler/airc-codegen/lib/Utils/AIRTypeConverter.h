#ifndef AIR_CONVERSION_UTILS_AIRTYPECONVERTER_H
#define AIR_CONVERSION_UTILS_AIRTYPECONVERTER_H

#include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.h"

#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/Transforms/DialectConversion.h"

namespace mlir::air {

// Converts AIR types to their LLVM equivalents:
//   !air.int        -> i64
//   !air.bool       -> i1
//   !air.str        -> !llvm.struct<(ptr, i64)>
//   !air.void       -> !llvm.void
//   !air.func<..>   -> ptr  (opaque function pointer)
//   !air.struct<..> -> !llvm.struct<(converted_field_types...)>
class AIRTypeConverter : public TypeConverter {
public:
  explicit AIRTypeConverter(MLIRContext *ctx) {
    addConversion([](Type type) { return type; });
    addConversion([ctx](IntType) -> Type { return IntegerType::get(ctx, 64); });
    addConversion([ctx](BoolType) -> Type { return IntegerType::get(ctx, 1); });
    addConversion([ctx](StrType) -> Type {
      return LLVM::LLVMStructType::getLiteral(
          ctx, {LLVM::LLVMPointerType::get(ctx), IntegerType::get(ctx, 64)});
    });
    addConversion([](VoidType type) -> Type {
      return LLVM::LLVMVoidType::get(type.getContext());
    });
    addConversion(
        [ctx](FuncType) -> Type { return LLVM::LLVMPointerType::get(ctx); });
    addConversion([this](StructType type) -> std::optional<Type> {
      SmallVector<Type> converted;
      for (Type ft : type.getFieldTypes()) {
        auto c = convertType(ft);
        if (!c)
          return std::nullopt;
        converted.push_back(c);
      }
      return LLVM::LLVMStructType::getLiteral(type.getContext(), converted);
    });

    addSourceMaterialization(
        [](OpBuilder &b, Type t, ValueRange vs, Location loc) -> Value {
          if (vs.size() != 1)
            return {};
          return UnrealizedConversionCastOp::create(b, loc, t, vs).getResult(0);
        });
    addTargetMaterialization(
        [](OpBuilder &b, Type t, ValueRange vs, Location loc) -> Value {
          if (vs.size() != 1)
            return {};
          return UnrealizedConversionCastOp::create(b, loc, t, vs).getResult(0);
        });
  }
};

} // namespace mlir::air

#endif // AIR_CONVERSION_UTILS_AIRTYPECONVERTER_H
