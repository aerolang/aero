use airc_syntax::ast;

#[allow(unused_imports, clippy::all)]
#[rustfmt::skip]
mod air_ast_generated;
mod serialize;

#[cxx::bridge(namespace = "airc::codegen")]
mod ffi {
    struct Diag {
        line: u32,
        col: u32,
        message: String,
    }

    struct CompileResult {
        ok: bool,
        diags: Vec<Diag>,
    }

    unsafe extern "C++" {
        include!("compiler/airc-codegen/include/codegen.h");
        fn compile_air_bytes(msg: &[u8], output_path: &str, support_path: &str) -> CompileResult;
    }
}

#[derive(Debug)]
pub struct Diag {
    pub line: u32,
    pub col: u32,
    pub message: String,
}

#[derive(Debug)]
pub struct CompileResult {
    pub ok: bool,
    pub diags: Vec<Diag>,
}

pub fn compile_air_ast(
    sources: Vec<(&ast::Source, &str)>,
    output_path: &str,
    support_path: &str,
) -> CompileResult {
    let buf = serialize::build_source(&sources);
    let r = ffi::compile_air_bytes(&buf, output_path, support_path);
    CompileResult {
        ok: r.ok,
        diags: r
            .diags
            .into_iter()
            .map(|d| Diag {
                line: d.line,
                col: d.col,
                message: d.message,
            })
            .collect(),
    }
}
