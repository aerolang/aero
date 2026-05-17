#include "compiler/airc-codegen/include/Dialect/AIR/AIROps.h"
#include "compiler/airc-codegen/include/Dialect/AIR/AIRTypes.h"

#include "mlir/IR/Builders.h"
#include "mlir/IR/OpImplementation.h"
#include "mlir/Interfaces/FunctionImplementation.h"

using namespace mlir;
using namespace mlir::air;

#define GET_ENUM_CLASSES
#include "compiler/airc-codegen/lib/Dialect/AIR/AIREnums.cpp.inc"

#define GET_OP_CLASSES
#include "compiler/airc-codegen/lib/Dialect/AIR/AIROps.cpp.inc"

//===----------------------------------------------------------------------===//
// Folders
//===----------------------------------------------------------------------===//

OpFoldResult ConstantOp::fold(FoldAdaptor adaptor) { return getValueAttr(); }

//===----------------------------------------------------------------------===//
// Verifiers
//===----------------------------------------------------------------------===//

LogicalResult ConstantOp::verify() {
  Type resultTy = getResult().getType();
  Attribute val = getValue();

  if (isa<IntType>(resultTy) && !isa<IntegerAttr>(val))
    return emitOpError("integer constant must have an IntegerAttr value");
  if (isa<BoolType>(resultTy) && !isa<IntegerAttr>(val))
    return emitOpError("bool constant must have an IntegerAttr value");
  if (isa<StrType>(resultTy) && !isa<StringAttr>(val))
    return emitOpError("string constant must have a StringAttr value");

  return success();
}

LogicalResult IfOp::verify() {
  // Both branches must terminate with air.yield carrying the same type
  // as the op's result (or no value if the result is absent).
  auto checkBranch = [&](Region &region, StringRef name) -> LogicalResult {
    if (region.empty())
      return emitOpError() << name << " region must not be empty";
    Block &block = region.front();
    if (block.empty())
      return emitOpError() << name << " block must not be empty";
    auto yieldOp = dyn_cast<YieldOp>(block.back());
    if (!yieldOp)
      return emitOpError() << name << " block must terminate with air.yield";

    bool hasResult = static_cast<bool>(getResult());
    bool hasYieldVal = static_cast<bool>(yieldOp.getValue());

    if (hasResult != hasYieldVal)
      return emitOpError() << name << " yield must "
                           << (hasResult ? "provide" : "omit")
                           << " a value to match the op result";

    if (hasResult && yieldOp.getValue().getType() != getResult().getType())
      return emitOpError() << name << " yield type "
                           << yieldOp.getValue().getType()
                           << " does not match result type "
                           << getResult().getType();
    return success();
  };

  if (failed(checkBranch(getThenRegion(), "then")))
    return failure();
  if (failed(checkBranch(getElseRegion(), "else")))
    return failure();
  return success();
}

LogicalResult ConstructOp::verify() {
  auto structTy = mlir::cast<StructType>(getResult().getType());
  auto fieldTypes = structTy.getFieldTypes();
  auto fields = getFields();
  if (fields.size() != fieldTypes.size())
    return emitOpError("expected ")
           << fieldTypes.size() << " fields but got " << fields.size();
  for (size_t i = 0; i < fields.size(); ++i) {
    if (fields[i].getType() != fieldTypes[i])
      return emitOpError("field ")
             << i << " type mismatch: expected " << fieldTypes[i] << " but got "
             << fields[i].getType();
  }
  return success();
}

LogicalResult FieldOp::verify() {
  auto structTy = mlir::cast<StructType>(getInput().getType());
  auto fieldTypes = structTy.getFieldTypes();
  int64_t idx = getIndex();
  if (idx < 0 || static_cast<size_t>(idx) >= fieldTypes.size())
    return emitOpError("index ") << idx << " out of bounds for struct with "
                                 << fieldTypes.size() << " fields";
  if (getResult().getType() != fieldTypes[idx])
    return emitOpError("result type ")
           << getResult().getType() << " does not match field type "
           << fieldTypes[idx];
  return success();
}

//===----------------------------------------------------------------------===//
// Printers and parsers
//===----------------------------------------------------------------------===//

// air.constant <value> : <type>
void ConstantOp::print(OpAsmPrinter &p) {
  p << "(";
  p.printAttribute(getValue());
  p << ") : ";
  p.printType(getResult().getType());
}

ParseResult ConstantOp::parse(OpAsmParser &parser, OperationState &result) {
  Attribute value;
  Type resultType;
  if (parser.parseLParen() || parser.parseAttribute(value) ||
      parser.parseRParen() || parser.parseColonType(resultType))
    return failure();
  result.addAttribute("value", value);
  result.addTypes(resultType);
  return success();
}

// air.if %cond { ... } else { ... } : <type>
void IfOp::print(OpAsmPrinter &p) {
  p << ' ';
  p.printOperand(getCondition());
  p << ' ';
  p.printRegion(getThenRegion(), /*printEntryBlockArgs=*/false);
  p << " else ";
  p.printRegion(getElseRegion(), /*printEntryBlockArgs=*/false);
  if (getResult())
    p << " : " << getResult().getType();
  p.printOptionalAttrDict((*this)->getAttrs());
}

ParseResult IfOp::parse(OpAsmParser &parser, OperationState &result) {
  OpAsmParser::UnresolvedOperand cond;
  Type boolType = BoolType::get(parser.getContext());
  if (parser.parseOperand(cond) ||
      parser.resolveOperand(cond, boolType, result.operands))
    return failure();

  auto *thenRegion = result.addRegion();
  if (parser.parseRegion(*thenRegion, {}))
    return failure();

  if (parser.parseKeyword("else"))
    return failure();
  auto *elseRegion = result.addRegion();
  if (parser.parseRegion(*elseRegion, {}))
    return failure();

  Type resultType;
  if (succeeded(parser.parseOptionalColon())) {
    if (parser.parseType(resultType))
      return failure();
    result.addTypes(resultType);
  }

  return success();
}

// air.func_def @name(%arg0: !air.int) -> !air.str { ... }
void FuncDefOp::print(OpAsmPrinter &p) {
  function_interface_impl::printFunctionOp(
      p, *this, /*isVariadic=*/false, "function_type", getArgAttrsAttrName(),
      getResAttrsAttrName());
}

ParseResult FuncDefOp::parse(OpAsmParser &parser, OperationState &result) {
  auto buildFuncType =
      [](Builder &b, ArrayRef<Type> argTypes, ArrayRef<Type> retTypes,
         function_interface_impl::VariadicFlag,
         std::string &) { return b.getFunctionType(argTypes, retTypes); };
  return function_interface_impl::parseFunctionOp(
      parser, result, /*allowVariadic=*/false,
      getFunctionTypeAttrName(result.name), buildFuncType,
      getArgAttrsAttrName(result.name), getResAttrsAttrName(result.name));
}

// air.struct_def @point(!air.int, !air.int)
void StructDefOp::print(OpAsmPrinter &p) {
  p << " @" << getSymName() << '(';
  llvm::interleaveComma(getFieldTypes(), p, [&](Attribute a) {
    p.printType(mlir::cast<TypeAttr>(a).getValue());
  });
  p << ')';
  p.printOptionalAttrDict((*this)->getAttrs(),
                          {getSymNameAttrName(), getFieldTypesAttrName()});
}

ParseResult StructDefOp::parse(OpAsmParser &parser, OperationState &result) {
  StringAttr name;
  if (parser.parseSymbolName(name, getSymNameAttrName(result.name),
                             result.attributes))
    return failure();
  SmallVector<Type> fieldTypes;
  if (parser.parseLParen() || parser.parseTypeList(fieldTypes) ||
      parser.parseRParen())
    return failure();
  SmallVector<Attribute> typeAttrs;
  for (Type t : fieldTypes)
    typeAttrs.push_back(TypeAttr::get(t));
  result.addAttribute(getFieldTypesAttrName(result.name),
                      ArrayAttr::get(parser.getContext(), typeAttrs));
  return parser.parseOptionalAttrDict(result.attributes);
}

// air.construct @point(%x, %y) : (!air.int, !air.int) -> !air.struct<!air.int,
// !air.int>
void ConstructOp::print(OpAsmPrinter &p) {
  p << ' ' << getStructNameAttr() << '(';
  p.printOperands(getFields());
  p << ") : (";
  llvm::interleaveComma(getFields(), p,
                        [&](Value v) { p.printType(v.getType()); });
  p << ") -> ";
  p.printType(getResult().getType());
  p.printOptionalAttrDict((*this)->getAttrs(), {getStructNameAttrName()});
}

ParseResult ConstructOp::parse(OpAsmParser &parser, OperationState &result) {
  FlatSymbolRefAttr structName;
  if (parser.parseAttribute(structName, getStructNameAttrName(result.name),
                            result.attributes))
    return failure();
  SmallVector<OpAsmParser::UnresolvedOperand> operands;
  if (parser.parseLParen() || parser.parseOperandList(operands) ||
      parser.parseRParen() || parser.parseColon() || parser.parseLParen())
    return failure();
  SmallVector<Type> argTypes;
  if (parser.parseTypeList(argTypes) || parser.parseRParen() ||
      parser.parseArrow())
    return failure();
  Type resultType;
  if (parser.parseType(resultType))
    return failure();
  if (parser.resolveOperands(operands, argTypes, parser.getCurrentLocation(),
                             result.operands))
    return failure();
  result.addTypes(resultType);
  return parser.parseOptionalAttrDict(result.attributes);
}

// air.field %p[0] : (!air.struct<!air.int, !air.int>) -> !air.int
void FieldOp::print(OpAsmPrinter &p) {
  p << ' ';
  p.printOperand(getInput());
  p << '[' << getIndex() << "] : (";
  p.printType(getInput().getType());
  p << ") -> ";
  p.printType(getResult().getType());
  p.printOptionalAttrDict((*this)->getAttrs(), {getIndexAttrName()});
}

ParseResult FieldOp::parse(OpAsmParser &parser, OperationState &result) {
  OpAsmParser::UnresolvedOperand input;
  if (parser.parseOperand(input))
    return failure();
  int64_t index;
  if (parser.parseLSquare() || parser.parseInteger(index) ||
      parser.parseRSquare())
    return failure();
  result.addAttribute(getIndexAttrName(result.name),
                      parser.getBuilder().getI64IntegerAttr(index));
  if (parser.parseColon() || parser.parseLParen())
    return failure();
  Type inputType;
  if (parser.parseType(inputType) || parser.parseRParen() ||
      parser.parseArrow())
    return failure();
  Type resultType;
  if (parser.parseType(resultType))
    return failure();
  if (parser.resolveOperand(input, inputType, result.operands))
    return failure();
  result.addTypes(resultType);
  return parser.parseOptionalAttrDict(result.attributes);
}

// air.func_ref @name : !air.func<(inputs) -> outputs>
void FuncRefOp::print(OpAsmPrinter &p) {
  p << ' ' << getCalleeAttr() << " : ";
  p.printType(getResult().getType());
  p.printOptionalAttrDict((*this)->getAttrs(), {getCalleeAttrName()});
}

ParseResult FuncRefOp::parse(OpAsmParser &parser, OperationState &result) {
  FlatSymbolRefAttr callee;
  Type resultType;
  if (parser.parseAttribute(callee, getCalleeAttrName(result.name),
                            result.attributes) ||
      parser.parseColonType(resultType))
    return failure();
  result.addTypes(resultType);
  return parser.parseOptionalAttrDict(result.attributes);
}

// air.indirect_call %callee(%args) : (arg_types) -> result_type
void IndirectCallOp::print(OpAsmPrinter &p) {
  p << ' ';
  p.printOperand(getCallee());
  p << '(';
  p.printOperands(getArgs());
  p << ") : (";
  llvm::interleaveComma(getArgs(), p,
                        [&](Value v) { p.printType(v.getType()); });
  p << ") -> ";
  if (getResult())
    p.printType(getResult().getType());
  else
    p << "()";
  p.printOptionalAttrDict((*this)->getAttrs());
}

ParseResult IndirectCallOp::parse(OpAsmParser &parser, OperationState &result) {
  OpAsmParser::UnresolvedOperand callee;
  if (parser.parseOperand(callee))
    return failure();
  SmallVector<OpAsmParser::UnresolvedOperand> args;
  if (parser.parseLParen() || parser.parseOperandList(args) ||
      parser.parseRParen() || parser.parseColon() || parser.parseLParen())
    return failure();
  SmallVector<Type> argTypes;
  if (parser.parseTypeList(argTypes) || parser.parseRParen() ||
      parser.parseArrow())
    return failure();
  SmallVector<Type> resultTypes;
  if (failed(parser.parseOptionalLParen())) {
    Type resultType;
    if (parser.parseType(resultType))
      return failure();
    resultTypes.push_back(resultType);
  } else {
    if (parser.parseRParen())
      return failure();
  }
  auto calleeType =
      FuncType::get(parser.getContext(), llvm::SmallVector<Type>(argTypes),
                    llvm::SmallVector<Type>(resultTypes));
  if (parser.resolveOperand(callee, calleeType, result.operands) ||
      parser.resolveOperands(args, argTypes, parser.getCurrentLocation(),
                             result.operands))
    return failure();
  result.addTypes(resultTypes);
  return parser.parseOptionalAttrDict(result.attributes);
}
