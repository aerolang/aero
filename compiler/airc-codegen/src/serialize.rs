//! Converts the AST from airc_syntax into the FlatBuffers AST which the C++
//! MLIR code uses.

use airc_syntax::ast;
use flatbuffers::FlatBufferBuilder;

use crate::air_ast_generated::air_ast as fbs;

fn make_loc(pos: (u32, u32)) -> fbs::Loc {
    fbs::Loc::new(pos.0, pos.1)
}

fn build_ty<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    ty: &ast::TypeData,
) -> (fbs::Ty, flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>) {
    match ty {
        ast::TypeData::Void => {
            let t = fbs::VoidTy::create(builder, &fbs::VoidTyArgs {});
            (fbs::Ty::VoidTy, t.as_union_value())
        }
        ast::TypeData::Int => {
            let t = fbs::IntTy::create(builder, &fbs::IntTyArgs {});
            (fbs::Ty::IntTy, t.as_union_value())
        }
        ast::TypeData::Str => {
            let t = fbs::StrTy::create(builder, &fbs::StrTyArgs {});
            (fbs::Ty::StrTy, t.as_union_value())
        }
        ast::TypeData::Bool => {
            let t = fbs::BoolTy::create(builder, &fbs::BoolTyArgs {});
            (fbs::Ty::BoolTy, t.as_union_value())
        }
        ast::TypeData::Struct(name) => {
            let name_off = builder.create_string(name);
            let t = fbs::StructTy::create(
                builder,
                &fbs::StructTyArgs {
                    value: Some(name_off),
                },
            );
            (fbs::Ty::StructTy, t.as_union_value())
        }
    }
}

fn build_simple_leaf<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    sd: &ast::SimpleData<'_>,
) -> flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset> {
    match sd {
        ast::SimpleData::Void => {
            fbs::VoidLit::create(builder, &fbs::VoidLitArgs {}).as_union_value()
        }
        ast::SimpleData::Bool(b) => {
            fbs::BoolLit::create(builder, &fbs::BoolLitArgs { value: *b }).as_union_value()
        }
        ast::SimpleData::Int(n) => {
            fbs::IntLit::create(builder, &fbs::IntLitArgs { value: *n }).as_union_value()
        }
        ast::SimpleData::Str(s) => {
            let off = builder.create_string(s);
            fbs::StrLit::create(builder, &fbs::StrLitArgs { value: Some(off) }).as_union_value()
        }
        ast::SimpleData::VarName(n) => {
            let off = builder.create_string(n);
            fbs::VarName::create(builder, &fbs::VarNameArgs { value: Some(off) }).as_union_value()
        }
        ast::SimpleData::DefName(n) => {
            let off = builder.create_string(n);
            fbs::DefName::create(builder, &fbs::DefNameArgs { value: Some(off) }).as_union_value()
        }
        ast::SimpleData::FieldAccess { var, index } => {
            let off = builder.create_string(var);
            fbs::FieldAccess::create(
                builder,
                &fbs::FieldAccessArgs {
                    var_name: Some(off),
                    index: *index,
                },
            )
            .as_union_value()
        }
    }
}

fn simple_expr_union(sd: &ast::SimpleData) -> fbs::SimpleExprData {
    match sd {
        ast::SimpleData::Void => fbs::SimpleExprData::VoidLit,
        ast::SimpleData::Bool(_) => fbs::SimpleExprData::BoolLit,
        ast::SimpleData::Int(_) => fbs::SimpleExprData::IntLit,
        ast::SimpleData::Str(_) => fbs::SimpleExprData::StrLit,
        ast::SimpleData::VarName(_) => fbs::SimpleExprData::VarName,
        ast::SimpleData::DefName(_) => fbs::SimpleExprData::DefName,
        ast::SimpleData::FieldAccess { .. } => fbs::SimpleExprData::FieldAccess,
    }
}

fn expr_union(sd: &ast::SimpleData) -> fbs::ExprData {
    match sd {
        ast::SimpleData::Void => fbs::ExprData::VoidLit,
        ast::SimpleData::Bool(_) => fbs::ExprData::BoolLit,
        ast::SimpleData::Int(_) => fbs::ExprData::IntLit,
        ast::SimpleData::Str(_) => fbs::ExprData::StrLit,
        ast::SimpleData::VarName(_) => fbs::ExprData::VarName,
        ast::SimpleData::DefName(_) => fbs::ExprData::DefName,
        ast::SimpleData::FieldAccess { .. } => fbs::ExprData::FieldAccess,
    }
}

fn build_simple_expr<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    sd: &ast::SimpleData<'_>,
    pos: (u32, u32),
) -> flatbuffers::WIPOffset<fbs::SimpleExpr<'t>> {
    let loc = make_loc(pos);
    let data_type = simple_expr_union(sd);
    let data = build_simple_leaf(builder, sd);
    fbs::SimpleExpr::create(
        builder,
        &fbs::SimpleExprArgs {
            loc: Some(&loc),
            data_type,
            data: Some(data),
        },
    )
}

fn build_expr_data<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    expr: &ast::Expr<'_>,
) -> (
    fbs::ExprData,
    flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>,
) {
    match &expr.data {
        ast::ExprData::Simple(sd) => (expr_union(sd), build_simple_leaf(builder, sd)),
        ast::ExprData::Call(call) => {
            let (callee_type, callee) = match &call.callee.data {
                ast::CalleeData::Builtin(n) => {
                    let off = builder.create_string(n);
                    let t = fbs::Builtin::create(builder, &fbs::BuiltinArgs { value: Some(off) });
                    (fbs::Callee::Builtin, t.as_union_value())
                }
                ast::CalleeData::DefName(n) => {
                    let off = builder.create_string(n);
                    let t = fbs::DefName::create(builder, &fbs::DefNameArgs { value: Some(off) });
                    (fbs::Callee::DefName, t.as_union_value())
                }
                ast::CalleeData::VarName(n) => {
                    let off = builder.create_string(n);
                    let t = fbs::VarName::create(builder, &fbs::VarNameArgs { value: Some(off) });
                    (fbs::Callee::VarName, t.as_union_value())
                }
            };
            let args: Vec<_> = call
                .args
                .iter()
                .map(|arg| build_simple_expr(builder, &arg.data, arg.span.pos))
                .collect();
            let args_off = builder.create_vector(&args);
            let call_data = fbs::CallData::create(
                builder,
                &fbs::CallDataArgs {
                    callee_type,
                    callee: Some(callee),
                    args: Some(args_off),
                },
            );
            (fbs::ExprData::CallData, call_data.as_union_value())
        }
        ast::ExprData::If(if_data) => {
            let if_off = build_if(builder, if_data);
            (fbs::ExprData::IfData, if_off.as_union_value())
        }
        ast::ExprData::Construct { name, fields } => {
            let fields_vec: Vec<_> = fields
                .iter()
                .map(|f| build_simple_expr(builder, &f.data, f.span.pos))
                .collect();
            let fields_off = builder.create_vector(&fields_vec);
            let name_off = name.map(|n| builder.create_string(n));
            let construct = fbs::ConstructData::create(
                builder,
                &fbs::ConstructDataArgs {
                    fields: Some(fields_off),
                    struct_name: name_off,
                },
            );
            (fbs::ExprData::ConstructData, construct.as_union_value())
        }
    }
}

fn build_block<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    body: &ast::Block<'_>,
) -> flatbuffers::WIPOffset<fbs::Block<'t>> {
    let assigns: Vec<_> = body
        .assigns
        .iter()
        .map(|a| build_assign(builder, a))
        .collect();
    let assigns_off = builder.create_vector(&assigns);
    let (result_type, result) = build_expr_data(builder, &body.result);
    fbs::Block::create(
        builder,
        &fbs::BlockArgs {
            assigns: Some(assigns_off),
            result_type,
            result: Some(result),
        },
    )
}

fn build_if<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    d: &ast::IfData<'_>,
) -> flatbuffers::WIPOffset<fbs::IfData<'t>> {
    let cond = build_simple_expr(builder, &d.cond.data, d.cond.span.pos);
    let then_block = build_block(builder, d.then_block.as_ref());
    let else_block = build_block(builder, d.else_block.as_ref());
    fbs::IfData::create(
        builder,
        &fbs::IfDataArgs {
            cond: Some(cond),
            then_block: Some(then_block),
            else_block: Some(else_block),
        },
    )
}

fn build_assign<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    assign: &ast::Assign<'_>,
) -> flatbuffers::WIPOffset<fbs::Assign<'t>> {
    let var_name = match &assign.target {
        ast::AssignTarget::Discard => builder.create_string(""),
        ast::AssignTarget::Var(v) => builder.create_string(v.value),
    };
    let loc = make_loc(assign.span.pos);
    let (expr_type, expr) = build_expr_data(builder, &assign.expr);
    fbs::Assign::create(
        builder,
        &fbs::AssignArgs {
            var_name: Some(var_name),
            loc: Some(&loc),
            expr_type,
            expr: Some(expr),
        },
    )
}

fn build_main_def<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    is_pub: bool,
    body: &ast::Block<'_>,
) -> (
    fbs::DefData,
    flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>,
) {
    let body_off = build_block(builder, body);
    let md = fbs::MainDef::create(
        builder,
        &fbs::MainDefArgs {
            is_pub,
            body: Some(body_off),
        },
    );
    (fbs::DefData::MainDef, md.as_union_value())
}

fn build_func_def<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    is_pub: bool,
    name: &ast::DefName<'_>,
    params: &[ast::Param<'_>],
    return_type: &ast::Type<'_>,
    body: &ast::Block<'_>,
) -> (
    fbs::DefData,
    flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>,
) {
    let name_off = builder.create_string(name.value);
    let params_vec: Vec<_> = params
        .iter()
        .map(|p| {
            let pname = builder.create_string(p.name.value);
            let (ty_type, ty) = build_ty(builder, &p.ty.data);
            fbs::FuncParam::create(
                builder,
                &fbs::FuncParamArgs {
                    name: Some(pname),
                    ty_type,
                    ty: Some(ty),
                },
            )
        })
        .collect();
    let params_off = builder.create_vector(&params_vec);
    let body_off = build_block(builder, body);
    let (return_type_type, return_type_val) = build_ty(builder, &return_type.data);
    let fd = fbs::FuncDef::create(
        builder,
        &fbs::FuncDefArgs {
            name: Some(name_off),
            is_pub,
            return_type_type,
            return_type: Some(return_type_val),
            params: Some(params_off),
            body: Some(body_off),
        },
    );
    (fbs::DefData::FuncDef, fd.as_union_value())
}

fn build_struct_def<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    is_pub: bool,
    name: &str,
    field_types: &[ast::Type<'_>],
) -> (
    fbs::DefData,
    flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>,
) {
    let name_off = builder.create_string(name);
    let fields_vec: Vec<_> = field_types
        .iter()
        .map(|ft| {
            let (ty_type, ty) = build_ty(builder, &ft.data);
            fbs::StructField::create(
                builder,
                &fbs::StructFieldArgs {
                    ty_type,
                    ty: Some(ty),
                },
            )
        })
        .collect();
    let fields_off = builder.create_vector(&fields_vec);
    let sd = fbs::StructDef::create(
        builder,
        &fbs::StructDefArgs {
            name: Some(name_off),
            is_pub,
            fields: Some(fields_off),
        },
    );
    (fbs::DefData::StructDef, sd.as_union_value())
}

fn build_def<'t>(
    builder: &mut FlatBufferBuilder<'t>,
    def: &ast::Def<'_>,
) -> flatbuffers::WIPOffset<fbs::Def<'t>> {
    let is_pub = matches!(def.vis, ast::Visibility::Pub);
    let loc = make_loc(def.span.pos);
    let (data_type, data) = match &def.data {
        ast::DefData::Main { body } => build_main_def(builder, is_pub, body),
        ast::DefData::Func {
            name,
            params,
            return_type,
            body,
        } => build_func_def(builder, is_pub, name, params, return_type, body),
        ast::DefData::Struct { name, field_types } => {
            build_struct_def(builder, is_pub, name, field_types)
        }
    };
    fbs::Def::create(
        builder,
        &fbs::DefArgs {
            loc: Some(&loc),
            data_type,
            data: Some(data),
        },
    )
}

pub fn build_source(sources: &[(&ast::Source, &str)]) -> Vec<u8> {
    let mut builder = FlatBufferBuilder::with_capacity(4096);
    let filename_off = builder.create_string(sources.first().map(|(_, f)| *f).unwrap_or(""));
    let all_defs: Vec<_> = sources
        .iter()
        .flat_map(|(src, _)| src.data.iter())
        .map(|def| build_def(&mut builder, def))
        .collect();
    let defs_off = builder.create_vector(&all_defs);
    let source = fbs::Source::create(
        &mut builder,
        &fbs::SourceArgs {
            filename: Some(filename_off),
            defs: Some(defs_off),
        },
    );
    builder.finish(source, None);
    builder.finished_data().to_vec()
}
