#include "compiler/airc-codegen/include/MLIRGen.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.h"
#include "compiler/airc-codegen/schema/air_ast_generated.h"

#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"

#include <string>
#include <vector>

namespace mlir {
void registerBuiltinDialectTranslation(DialectRegistry &);
void registerLLVMDialectTranslation(DialectRegistry &);
} // namespace mlir

using namespace mlir::air;
using namespace air_ast;

namespace airc::codegen {

//===----------------------------------------------------------------------===//
// MLIRGen context
//===----------------------------------------------------------------------===//

struct MLIRGenContext {
  mlir::MLIRContext &context;
  mlir::OpBuilder builder;
  mlir::Location loc;
  mlir::ModuleOp module;
  llvm::StringMap<mlir::Value> symbolTable;
  llvm::StringMap<StructType> structTable;
  bool hadError = false;

  MLIRGenContext(mlir::MLIRContext &ctx)
      : context(ctx), builder(&ctx), loc(mlir::UnknownLoc::get(&ctx)),
        module(mlir::ModuleOp::create(loc)) {
    builder.setInsertionPointToEnd(module.getBody());
  }

  mlir::Location makeLoc(const std::string &filename, uint32_t line,
                         uint32_t col) {
    return mlir::FileLineColLoc::get(
        &context, mlir::StringAttr::get(&context, filename), line, col);
  }

  mlir::Value lowerExpr(const Expr *expr, const std::string &filename);
  mlir::Value lowerSimple(const SimpleExpr *simple,
                          const std::string &filename);
  mlir::Value lowerBlock(const Block *block, const std::string &filename);
  mlir::Value lowerExprUnion(ExprData type, const void *data,
                             mlir::Location loc, const std::string &filename);
};

//===----------------------------------------------------------------------===//
// Type helper
//===----------------------------------------------------------------------===//

static mlir::Type
airTypeFromTy(mlir::MLIRContext *ctx, Ty kind, const void *tyTable,
              const llvm::StringMap<StructType> &structTable) {
  switch (kind) {
  case Ty::StrTy:
    return StrType::get(ctx);
  case Ty::IntTy:
    return IntType::get(ctx);
  case Ty::BoolTy:
    return BoolType::get(ctx);
  case Ty::VoidTy:
    return VoidType::get(ctx);
  case Ty::StructTy: {
    const auto *st = reinterpret_cast<const StructTy *>(tyTable);
    if (st && st->value()) {
      auto it = structTable.find(st->value()->str());
      if (it != structTable.end())
        return it->second;
    }
    return VoidType::get(ctx);
  }
  case Ty::NONE:
    return VoidType::get(ctx);
  }
  return VoidType::get(ctx);
}

//===----------------------------------------------------------------------===//
// Expression lowering
//===----------------------------------------------------------------------===//

mlir::Value MLIRGenContext::lowerSimple(const SimpleExpr *simple,
                                        const std::string &filename) {
  if (!simple)
    return {};
  auto simpleLoc = makeLoc(filename, simple->loc() ? simple->loc()->line() : 0,
                           simple->loc() ? simple->loc()->col() : 0);

  switch (simple->data_type()) {
  case SimpleExprData::VoidLit:
    return {};

  case SimpleExprData::BoolLit: {
    const auto *bv = simple->data_as_BoolLit();
    return ConstantOp::create(
               builder, simpleLoc, BoolType::get(&context),
               builder.getIntegerAttr(builder.getI1Type(), bv->value() ? 1 : 0))
        .getResult();
  }

  case SimpleExprData::IntLit: {
    const auto *iv = simple->data_as_IntLit();
    return ConstantOp::create(builder, simpleLoc, IntType::get(&context),
                              builder.getI64IntegerAttr(iv->value()))
        .getResult();
  }

  case SimpleExprData::StrLit: {
    const auto *sv = simple->data_as_StrLit();
    std::string s = sv->value() ? sv->value()->str() : "";
    return ConstantOp::create(builder, simpleLoc, StrType::get(&context),
                              builder.getStringAttr(s))
        .getResult();
  }

  case SimpleExprData::VarName: {
    const auto *vn = simple->data_as_VarName();
    std::string name = vn->value() ? vn->value()->str() : "";
    auto it = symbolTable.find(name);
    if (it != symbolTable.end())
      return it->second;
    mlir::emitError(simpleLoc) << "undefined variable: %" << name;
    hadError = true;
    return {};
  }

  case SimpleExprData::FieldAccess: {
    const auto *fa = simple->data_as_FieldAccess();
    std::string name = fa->var_name() ? fa->var_name()->str() : "";
    auto it = symbolTable.find(name);
    if (it == symbolTable.end()) {
      mlir::emitError(simpleLoc) << "undefined variable: $" << name;
      hadError = true;
      return {};
    }
    auto structVal = it->second;
    auto structTy = mlir::dyn_cast<StructType>(structVal.getType());
    if (!structTy) {
      mlir::emitError(simpleLoc) << "$" << name << " is not a struct";
      hadError = true;
      return {};
    }
    uint32_t idx = fa->index();
    if (idx >= structTy.getFieldTypes().size()) {
      mlir::emitError(simpleLoc) << "field index " << idx << " out of bounds";
      hadError = true;
      return {};
    }
    auto fieldType = structTy.getFieldTypes()[idx];
    return FieldOp::create(builder, simpleLoc, fieldType, structVal,
                           builder.getI64IntegerAttr(idx))
        .getResult();
  }

  case SimpleExprData::DefName: {
    const auto *dn = simple->data_as_DefName();
    std::string name = dn->value() ? dn->value()->str() : "";
    auto funcOp = mlir::SymbolTable::lookupSymbolIn(module, name);
    auto airFunc = mlir::dyn_cast_or_null<FuncDefOp>(funcOp);
    if (!airFunc) {
      mlir::emitError(simpleLoc) << "undefined function: %" << name;
      hadError = true;
      return {};
    }
    mlir::FunctionType ft = airFunc.getFunctionType();
    auto airFuncType =
        FuncType::get(&context, llvm::SmallVector<mlir::Type>(ft.getInputs()),
                      llvm::SmallVector<mlir::Type>(ft.getResults()));
    return FuncRefOp::create(builder, simpleLoc, airFuncType,
                             mlir::FlatSymbolRefAttr::get(&context, name))
        .getResult();
  }

  case SimpleExprData::NONE:
    return {};
  }
  return {};
}

mlir::Value MLIRGenContext::lowerBlock(const Block *block,
                                       const std::string &filename) {
  if (!block)
    return {};
  if (block->assigns()) {
    for (const auto *assign : *block->assigns()) {
      if (!assign)
        continue;
      std::string callee = assign->var_name() ? assign->var_name()->str() : "";
      mlir::Location assignLoc =
          makeLoc(filename, assign->loc() ? assign->loc()->line() : 0,
                  assign->loc() ? assign->loc()->col() : 0);
      auto value = lowerExprUnion(assign->expr_type(), assign->expr(),
                                  assignLoc, filename);
      if (value && !callee.empty())
        symbolTable[callee] = value;
    }
  }
  mlir::Location resultLoc = makeLoc(filename, 0, 0);
  return lowerExprUnion(block->result_type(), block->result(), resultLoc,
                        filename);
}

mlir::Value MLIRGenContext::lowerExpr(const Expr *expr,
                                      const std::string &filename) {
  if (!expr)
    return {};
  auto exprLoc = makeLoc(filename, expr->loc() ? expr->loc()->line() : 0,
                         expr->loc() ? expr->loc()->col() : 0);
  return lowerExprUnion(expr->data_type(), expr->data(), exprLoc, filename);
}

mlir::Value MLIRGenContext::lowerExprUnion(ExprData type, const void *data,
                                           mlir::Location loc,
                                           const std::string &filename) {
  switch (type) {
  case ExprData::VoidLit:
    return {};

  case ExprData::BoolLit: {
    const auto *bv = reinterpret_cast<const BoolLit *>(data);
    return ConstantOp::create(
               builder, loc, BoolType::get(&context),
               builder.getIntegerAttr(builder.getI1Type(), bv->value() ? 1 : 0))
        .getResult();
  }

  case ExprData::IntLit: {
    const auto *iv = reinterpret_cast<const IntLit *>(data);
    return ConstantOp::create(builder, loc, IntType::get(&context),
                              builder.getI64IntegerAttr(iv->value()))
        .getResult();
  }

  case ExprData::StrLit: {
    const auto *sv = reinterpret_cast<const StrLit *>(data);
    std::string s = sv->value() ? sv->value()->str() : "";
    return ConstantOp::create(builder, loc, StrType::get(&context),
                              builder.getStringAttr(s))
        .getResult();
  }

  case ExprData::VarName: {
    const auto *vn = reinterpret_cast<const VarName *>(data);
    std::string name = vn->value() ? vn->value()->str() : "";
    auto it = symbolTable.find(name);
    if (it != symbolTable.end())
      return it->second;
    mlir::emitError(loc) << "undefined variable: %" << name;
    hadError = true;
    return {};
  }

  case ExprData::DefName: {
    const auto *dn = reinterpret_cast<const DefName *>(data);
    std::string name = dn->value() ? dn->value()->str() : "";
    auto funcOp = mlir::SymbolTable::lookupSymbolIn(module, name);
    auto airFunc = mlir::dyn_cast_or_null<FuncDefOp>(funcOp);
    if (!airFunc) {
      mlir::emitError(loc) << "undefined function: %" << name;
      hadError = true;
      return {};
    }
    mlir::FunctionType ft = airFunc.getFunctionType();
    auto airFuncType =
        FuncType::get(&context, llvm::SmallVector<mlir::Type>(ft.getInputs()),
                      llvm::SmallVector<mlir::Type>(ft.getResults()));
    return FuncRefOp::create(builder, loc, airFuncType,
                             mlir::FlatSymbolRefAttr::get(&context, name))
        .getResult();
  }

  case ExprData::FieldAccess: {
    const auto *fa = reinterpret_cast<const FieldAccess *>(data);
    std::string name = fa->var_name() ? fa->var_name()->str() : "";
    auto it = symbolTable.find(name);
    if (it == symbolTable.end()) {
      mlir::emitError(loc) << "undefined variable: $" << name;
      hadError = true;
      return {};
    }
    auto structVal = it->second;
    auto structTy = mlir::dyn_cast<StructType>(structVal.getType());
    if (!structTy) {
      mlir::emitError(loc) << "$" << name << " is not a struct";
      hadError = true;
      return {};
    }
    uint32_t idx = fa->index();
    if (idx >= structTy.getFieldTypes().size()) {
      mlir::emitError(loc) << "field index " << idx << " out of bounds";
      hadError = true;
      return {};
    }
    auto fieldType = structTy.getFieldTypes()[idx];
    return FieldOp::create(builder, loc, fieldType, structVal,
                           builder.getI64IntegerAttr(idx))
        .getResult();
  }

  case ExprData::CallData: {
    const auto *call = reinterpret_cast<const CallData *>(data);
    if (!call)
      return {};
    std::string callee;
    bool isBuiltin = false, isVar = false;
    switch (call->callee_type()) {
    case Callee::Builtin:
      if (auto *n = call->callee_as_Builtin(); n && n->value())
        callee = n->value()->str();
      isBuiltin = true;
      break;
    case Callee::VarName:
      if (auto *n = call->callee_as_VarName(); n && n->value())
        callee = n->value()->str();
      isVar = true;
      break;
    case Callee::DefName:
      if (auto *n = call->callee_as_DefName(); n && n->value())
        callee = n->value()->str();
      break;
    default:
      break;
    }

    std::vector<mlir::Value> args;
    if (call->args()) {
      for (const auto *arg : *call->args()) {
        if (!arg)
          continue;
        mlir::Value v = lowerSimple(arg, filename);
        if (v)
          args.push_back(v);
      }
    }

    // Typed arithmetic / comparison ops (two-arg builtins)
    if (isBuiltin && args.size() == 2) {
      auto intType = IntType::get(&context);
      auto boolType = BoolType::get(&context);
      mlir::Value lhs = args[0], rhs = args[1];

      if (callee == "int/add")
        return AddOp::create(builder, loc, intType, lhs, rhs).getResult();
      if (callee == "int/sub")
        return SubOp::create(builder, loc, intType, lhs, rhs).getResult();
      if (callee == "int/mul")
        return MulOp::create(builder, loc, intType, lhs, rhs).getResult();
      if (callee == "int/div")
        return DivOp::create(builder, loc, intType, lhs, rhs).getResult();

      auto icmp = [&](ICmpPredicate pred) -> mlir::Value {
        return ICmpOp::create(builder, loc, boolType, pred, lhs, rhs)
            .getResult();
      };
      if (callee == "int/eq")
        return icmp(ICmpPredicate::eq);
      if (callee == "int/ne")
        return icmp(ICmpPredicate::ne);
      if (callee == "int/lt")
        return icmp(ICmpPredicate::lt);
      if (callee == "int/le")
        return icmp(ICmpPredicate::le);
      if (callee == "int/gt")
        return icmp(ICmpPredicate::gt);
      if (callee == "int/ge")
        return icmp(ICmpPredicate::ge);
    }

    // Runtime calls and user-defined function calls
    std::string funcName;
    mlir::TypeRange resultTypes;
    auto strType = StrType::get(&context);

    if (callee == "log") {
      funcName = "runtime$log";
    } else if (callee == "str/concat") {
      funcName = "builtins$str-concat";
      resultTypes = mlir::TypeRange{strType};
    } else if (callee == "str/free") {
      funcName = "builtins$str-free";
    } else if (callee == "int/as-str") {
      funcName = "builtins$int-as-str";
      resultTypes = mlir::TypeRange{strType};
    } else if (callee == "bool/as-str") {
      funcName = "builtins$bool-as-str";
      resultTypes = mlir::TypeRange{strType};
    } else if (isVar) {
      auto it = symbolTable.find(callee);
      if (it == symbolTable.end()) {
        mlir::emitError(loc) << "undefined variable: $" << callee;
        hadError = true;
        return {};
      }
      mlir::Value funcVal = it->second;
      auto airFuncType = mlir::dyn_cast<FuncType>(funcVal.getType());
      if (!airFuncType) {
        mlir::emitError(loc) << "$" << callee << " is not a function";
        hadError = true;
        return {};
      }
      mlir::TypeRange indirectResultTypes;
      if (!airFuncType.getOutputs().empty() &&
          !mlir::isa<VoidType>(airFuncType.getOutputs()[0]))
        indirectResultTypes = mlir::TypeRange{airFuncType.getOutputs()[0]};
      auto indirectCall = IndirectCallOp::create(
          builder, loc, indirectResultTypes, funcVal, args);
      return indirectCall.getNumResults() > 0 ? indirectCall.getResult()
                                              : mlir::Value{};
    } else if (!isBuiltin && !isVar) {
      funcName = callee;
      auto funcOp = mlir::SymbolTable::lookupSymbolIn(module, funcName);
      if (auto airFunc = mlir::dyn_cast_or_null<FuncDefOp>(funcOp)) {
        auto retTypes = airFunc.getFunctionType().getResults();
        if (!retTypes.empty() && !mlir::isa<VoidType>(retTypes[0]))
          resultTypes = mlir::TypeRange{retTypes[0]};
      }
    } else {
      mlir::emitError(loc) << "unknown callee: " << callee;
      hadError = true;
      return {};
    }

    auto callOp =
        CallOp::create(builder, loc, resultTypes,
                       mlir::FlatSymbolRefAttr::get(&context, funcName), args);
    return callOp.getNumResults() > 0 ? callOp.getResult() : mlir::Value{};
  }

  case ExprData::IfData: {
    const auto *ifData = reinterpret_cast<const IfData *>(data);
    if (!ifData)
      return {};

    mlir::Value condValue = lowerSimple(ifData->cond(), filename);
    if (!condValue)
      return {};

    auto savedSymbols = symbolTable;
    mlir::Region thenRegion, elseRegion;
    mlir::Value thenYieldVal, elseYieldVal;

    {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.createBlock(&thenRegion);
      thenYieldVal = lowerBlock(ifData->then_block(), filename);
      symbolTable = savedSymbols;
    }
    {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.createBlock(&elseRegion);
      elseYieldVal = lowerBlock(ifData->else_block(), filename);
      symbolTable = savedSymbols;
    }

    bool isVoid = !thenYieldVal;
    llvm::SmallVector<mlir::Type, 1> resultTypeVec;
    if (!isVoid)
      resultTypeVec.push_back(thenYieldVal.getType());

    auto ifOp = IfOp::create(builder, loc, resultTypeVec, condValue);

    auto populateRegion = [&](mlir::Region &src, mlir::Region &dst,
                              mlir::Value yieldVal) {
      dst.takeBody(src);
      mlir::OpBuilder b(&dst.front(), dst.front().end());
      YieldOp::create(b, loc, isVoid ? mlir::Value{} : yieldVal);
    };
    populateRegion(thenRegion, ifOp.getThenRegion(), thenYieldVal);
    populateRegion(elseRegion, ifOp.getElseRegion(), elseYieldVal);

    builder.setInsertionPointAfter(ifOp);
    return isVoid ? mlir::Value{} : ifOp.getResult();
  }

  case ExprData::ConstructData: {
    const auto *cd = reinterpret_cast<const ConstructData *>(data);
    if (!cd)
      return {};

    std::vector<mlir::Value> fieldVals;
    if (cd->fields()) {
      for (const auto *f : *cd->fields()) {
        if (!f)
          continue;
        auto v = lowerSimple(f, filename);
        if (v)
          fieldVals.push_back(v);
      }
    }

    StructType structTy;
    std::string resolvedName;
    if (cd->struct_name() && cd->struct_name()->size() > 0) {
      resolvedName = cd->struct_name()->str();
      auto it = structTable.find(resolvedName);
      if (it == structTable.end()) {
        mlir::emitError(loc) << "unknown struct: #" << resolvedName;
        hadError = true;
        return {};
      }
      structTy = it->second;
    } else {
      llvm::SmallVector<mlir::Type> fieldTypes;
      for (auto &v : fieldVals)
        fieldTypes.push_back(v.getType());
      for (auto &kv : structTable) {
        if (kv.second.getFieldTypes() ==
            llvm::ArrayRef<mlir::Type>(fieldTypes)) {
          structTy = kv.second;
          resolvedName = kv.first().str();
          break;
        }
      }
      if (!structTy) {
        mlir::emitError(loc)
            << "cannot infer struct type for construct expression";
        hadError = true;
        return {};
      }
    }

    return ConstructOp::create(
               builder, loc, structTy,
               mlir::FlatSymbolRefAttr::get(&context, resolvedName), fieldVals)
        .getResult();
  }

  case ExprData::NONE:
    return {};
  }
  return {};
}

//===----------------------------------------------------------------------===//
// Function body emission
//===----------------------------------------------------------------------===//

template <typename OpTy>
static bool emitFunctionBody(
    MLIRGenContext &cg, OpTy func, const std::vector<mlir::Type> &paramTypes,
    const flatbuffers::Vector<flatbuffers::Offset<FuncParam>> *params,
    const Block *body, bool returnsVoid, const std::string &filename) {
  auto *entryBlock = cg.builder.createBlock(&func.getBody());

  for (auto paramType : paramTypes)
    entryBlock->addArgument(paramType, cg.loc);

  cg.builder.setInsertionPointToStart(entryBlock);
  cg.symbolTable.clear();

  if (params) {
    for (size_t i = 0; i < params->size(); ++i) {
      const auto *p = (*params)[i];
      std::string name = p && p->name() ? p->name()->str() : "";
      cg.symbolTable[name] = entryBlock->getArgument(i);
    }
  }

  if (body && body->assigns()) {
    for (const auto *assign : *body->assigns()) {
      if (!assign)
        continue;
      std::string callee = assign->var_name() ? assign->var_name()->str() : "";
      mlir::Location assignLoc =
          assign->loc() ? cg.makeLoc(filename, assign->loc()->line(),
                                     assign->loc()->col())
                        : cg.loc;
      auto value = cg.lowerExprUnion(assign->expr_type(), assign->expr(),
                                     assignLoc, filename);
      if (value && !callee.empty())
        cg.symbolTable[callee] = value;
    }
  }

  mlir::Value resultValue;
  if (body) {
    mlir::Value v = cg.lowerExprUnion(body->result_type(), body->result(),
                                      cg.loc, filename);
    if (!returnsVoid)
      resultValue = v;
  }
  ReturnOp::create(cg.builder, cg.loc, resultValue);

  cg.builder.setInsertionPointToEnd(cg.module.getBody());
  return !cg.hadError;
}

//===----------------------------------------------------------------------===//
// Public entry point
//===----------------------------------------------------------------------===//

mlir::OwningOpRef<mlir::ModuleOp> mlirGen(mlir::MLIRContext &context,
                                          const Source *source,
                                          const MainDef *mainDef,
                                          const std::string &filename) {
  MLIRGenContext cg(context);

  if (!source->defs())
    return mlir::OwningOpRef<mlir::ModuleOp>(cg.module);

  // Pass 1: register all struct types so they're visible during lowering.
  for (const auto *def : *source->defs()) {
    if (!def || def->data_type() != DefData::StructDef)
      continue;
    const auto *sd = def->data_as_StructDef();
    if (!sd || !sd->name())
      continue;
    std::string sname = sd->name()->str();
    llvm::SmallVector<mlir::Type> fieldTypes;
    if (sd->fields()) {
      for (const auto *f : *sd->fields()) {
        if (!f)
          continue;
        fieldTypes.push_back(
            airTypeFromTy(&cg.context, f->ty_type(), f->ty(), cg.structTable));
      }
    }
    auto structTy = StructType::get(&cg.context, fieldTypes);
    cg.structTable[sname] = structTy;

    llvm::SmallVector<mlir::Attribute> typeAttrs;
    for (mlir::Type ft : fieldTypes)
      typeAttrs.push_back(mlir::TypeAttr::get(ft));
    StructDefOp::create(cg.builder, cg.loc, cg.builder.getStringAttr(sname),
                        cg.builder.getArrayAttr(typeAttrs));
  }

  // Pass 2: emit function definitions.
  for (const auto *def : *source->defs()) {
    if (!def || def->data_type() != DefData::FuncDef)
      continue;
    const auto *funcDef = def->data_as_FuncDef();
    if (!funcDef)
      continue;
    std::string funcName = funcDef->name() ? funcDef->name()->str() : "";

    std::vector<mlir::Type> paramTypes;
    if (funcDef->params()) {
      for (const auto *param : *funcDef->params()) {
        if (!param)
          continue;
        paramTypes.push_back(airTypeFromTy(&cg.context, param->ty_type(),
                                           param->ty(), cg.structTable));
      }
    }

    auto retType = airTypeFromTy(&cg.context, funcDef->return_type_type(),
                                 funcDef->return_type(), cg.structTable);
    auto funcType = cg.builder.getFunctionType(paramTypes, {retType});
    auto func = FuncDefOp::create(cg.builder, cg.loc,
                                  cg.builder.getStringAttr(funcName),
                                  mlir::TypeAttr::get(funcType),
                                  /*arg_attrs=*/nullptr, /*res_attrs=*/nullptr);

    bool returnsVoid = mlir::isa<VoidType>(retType);
    if (!emitFunctionBody(cg, func, paramTypes, funcDef->params(),
                          funcDef->body(), returnsVoid, filename))
      return {};
  }

  // Main entry point
  auto mainDefOp = MainDefOp::create(cg.builder, cg.loc);
  if (!emitFunctionBody(cg, mainDefOp, {}, nullptr, mainDef->body(),
                        /*returnsVoid=*/true, filename))
    return {};

  return mlir::OwningOpRef<mlir::ModuleOp>(cg.module);
}

} // namespace airc::codegen
