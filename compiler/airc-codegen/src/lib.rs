use airc_syntax::ast;

#[cxx::bridge(namespace = "airc::codegen")]
mod ffi {
    // Expose AST types to C++ (using owned strings for CXX compatibility)
    struct FuncInfo {
        name: String,
        is_pub: bool,
        log_message: String,  // Simplified for hello world
    }

    unsafe extern "C++" {
        include!("compiler/airc-codegen/include/codegen.h");

        fn compile_air_ast(funcs: Vec<FuncInfo>, output_path: &str, runtime_path: &str);
    }
}

// Extract function information from AST
pub fn extract_funcs<'a>(ast_source: &ast::Source<'a>) -> Vec<ffi::FuncInfo> {
    let mut funcs = Vec::new();

    for def in &ast_source.data {
        if let ast::DefData::Func { name, body, .. } = &def.data {
            let is_pub = matches!(def.vis, ast::Visibility::Pub);

            // For now, extract log message if the body result has a log call
            let log_message = match &body.result.data {
                ast::ExprData::Call(call_data) => {
                    if let ast::CalleeData::Name("log") = call_data.callee.data {
                        if let Some(arg) = call_data.args.first() {
                            if let ast::SimpleData::Str(s) = arg.data {
                                s.to_string()
                            } else {
                                String::new()
                            }
                        } else {
                            String::new()
                        }
                    } else {
                        String::new()
                    }
                }
                _ => String::new(),
            };

            funcs.push(ffi::FuncInfo {
                name: name.value.to_string(),
                is_pub,
                log_message,
            });
        }
    }

    funcs
}

pub fn compile_air_ast(funcs: Vec<ffi::FuncInfo>, output_path: &str, runtime_path: &str) {
    ffi::compile_air_ast(funcs, output_path, runtime_path);
}
