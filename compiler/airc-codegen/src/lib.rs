use airc_syntax::ast;

#[cxx::bridge(namespace = "airc::codegen")]
mod ffi {
    // Expose AST types to C++ (using owned strings for CXX compatibility)
    struct FuncInfo {
        name: String,
        is_pub: bool,
        log_messages: Vec<String>,  // All log statements in the function
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

            // Extract all log messages from assigns and result
            let mut log_messages = Vec::new();

            // Extract logs from assigns
            for assign in &body.assigns {
                if let Some(msg) = extract_log_message(&assign.expr) {
                    log_messages.push(msg);
                }
            }

            // Extract log from result expression
            if let Some(msg) = extract_log_message(&body.result) {
                log_messages.push(msg);
            }

            funcs.push(ffi::FuncInfo {
                name: name.value.to_string(),
                is_pub,
                log_messages,
            });
        }
    }

    funcs
}

fn extract_log_message(expr: &ast::Expr) -> Option<String> {
    match &expr.data {
        ast::ExprData::Call(call_data) => {
            if let ast::CalleeData::Name("log") = call_data.callee.data {
                if let Some(arg) = call_data.args.first() {
                    if let ast::SimpleData::Str(s) = arg.data {
                        return Some(s.to_string());
                    }
                }
            }
            None
        }
        _ => None,
    }
}

pub fn compile_air_ast(funcs: Vec<ffi::FuncInfo>, output_path: &str, runtime_path: &str) {
    ffi::compile_air_ast(funcs, output_path, runtime_path);
}
