use airc_syntax::ast;

#[cxx::bridge(namespace = "airc::codegen")]
mod ffi {
    // Expose AST types to C++ (using owned strings for CXX compatibility)
    struct SourceData {
        main_defs: Vec<MainDefData>,
        func_defs: Vec<FuncDefData>,
    }

    struct MainDefData {
        is_pub: bool,
        assigns: Vec<AssignData>,
        result: ExprData,
    }

    struct FuncDefData {
        name: String,
        is_pub: bool,
        params: Vec<ParamData>,
        assigns: Vec<AssignData>,
        result: ExprData,
    }

    struct ParamData {
        name: String,
        ty: String,  // "Int", "Str", "Void"
    }

    struct AssignData {
        var_name: String,
        expr: ExprData,
    }

    struct ExprData {
        kind: String,  // "call" or "simple"
        // For calls
        callee: String,
        args: Vec<SimpleData>,
        // For simple exprs
        simple: SimpleData,
    }

    struct SimpleData {
        kind: String,  // "void", "str", "sym", "varname", "defname"
        value: String, // The actual value (empty for void)
    }

    unsafe extern "C++" {
        include!("compiler/airc-codegen/include/codegen.h");

        fn compile_air_ast(sources: Vec<SourceData>, output_path: &str, runtime_path: &str);
    }
}

// Convert AST to FFI types
fn convert_ast_to_ffi(ast_source: &ast::Source) -> ffi::SourceData {
    let mut main_defs = Vec::new();
    let mut func_defs = Vec::new();

    for def in &ast_source.data {
        let is_pub = matches!(def.vis, ast::Visibility::Pub);

        match &def.data {
            ast::DefData::Main { body } => {
                let assigns = body.assigns.iter().map(|assign| ffi::AssignData {
                    var_name: assign.var.value.to_string(),
                    expr: convert_expr(&assign.expr),
                }).collect();

                let result = convert_expr(&body.result);

                main_defs.push(ffi::MainDefData {
                    is_pub,
                    assigns,
                    result,
                });
            }
            ast::DefData::Func { name, params, body, .. } => {
                let params_data = params.iter().map(|param| {
                    let ty_str = match param.ty.data {
                        ast::TypeData::Int => "Int",
                        ast::TypeData::Str => "Str",
                        ast::TypeData::Void => "Void",
                    };
                    ffi::ParamData {
                        name: param.name.value.to_string(),
                        ty: ty_str.to_string(),
                    }
                }).collect();

                let assigns = body.assigns.iter().map(|assign| ffi::AssignData {
                    var_name: assign.var.value.to_string(),
                    expr: convert_expr(&assign.expr),
                }).collect();

                let result = convert_expr(&body.result);

                func_defs.push(ffi::FuncDefData {
                    name: name.value.to_string(),
                    is_pub,
                    params: params_data,
                    assigns,
                    result,
                });
            }
        }
    }

    ffi::SourceData { main_defs, func_defs }
}

fn convert_expr(expr: &ast::Expr) -> ffi::ExprData {
    match &expr.data {
        ast::ExprData::Call(call_data) => {
            let callee = match &call_data.callee.data {
                ast::CalleeData::Name(n) => n.to_string(),
                ast::CalleeData::VarName(n) => format!("%{}", n),
                ast::CalleeData::DefName(n) => format!("${}", n),
            };

            let args = call_data.args.iter().map(|arg| convert_simple(&arg.data)).collect();

            ffi::ExprData {
                kind: "call".to_string(),
                callee,
                args,
                simple: ffi::SimpleData {
                    kind: "void".to_string(),
                    value: String::new(),
                },
            }
        }
        ast::ExprData::Simple(simple_data) => {
            ffi::ExprData {
                kind: "simple".to_string(),
                callee: String::new(),
                args: Vec::new(),
                simple: convert_simple(simple_data),
            }
        }
    }
}

fn convert_simple(simple_data: &ast::SimpleData) -> ffi::SimpleData {
    match simple_data {
        ast::SimpleData::Void => ffi::SimpleData {
            kind: "void".to_string(),
            value: String::new(),
        },
        ast::SimpleData::Str(s) => ffi::SimpleData {
            kind: "str".to_string(),
            value: s.to_string(),
        },
        ast::SimpleData::Sym(s) => ffi::SimpleData {
            kind: "sym".to_string(),
            value: s.to_string(),
        },
        ast::SimpleData::Name(n) => ffi::SimpleData {
            kind: "name".to_string(),
            value: n.to_string(),
        },
        ast::SimpleData::VarName(n) => ffi::SimpleData {
            kind: "varname".to_string(),
            value: n.to_string(),
        },
        ast::SimpleData::DefName(n) => ffi::SimpleData {
            kind: "defname".to_string(),
            value: n.to_string(),
        },
    }
}

pub fn compile_air_ast(sources: Vec<&ast::Source>, output_path: &str, runtime_path: &str) {
    let source_data: Vec<ffi::SourceData> = sources
        .iter()
        .map(|source| convert_ast_to_ffi(source))
        .collect();
    ffi::compile_air_ast(source_data, output_path, runtime_path);
}
