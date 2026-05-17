use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{self, Config};

/// Compile an AIR source string to an executable.
pub fn compile_air(
    source: &str,
    filename: &str,
    output_path: &str,
    support_path: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let ast = airc_syntax::parse_air(source).map_err(|e| {
        let (line, col) = e.line_col();
        render_diagnostic(source, filename, line, col, &e.message())
    })?;

    let result = airc_codegen::compile_air_ast(vec![(&ast, filename)], output_path, support_path);

    if result.ok {
        return Ok(());
    }

    let rendered: Vec<String> = result
        .diags
        .iter()
        .map(|d| {
            render_diagnostic(
                source,
                filename,
                d.line as usize,
                d.col as usize,
                &d.message,
            )
        })
        .collect();

    Err(rendered.join("\n").into())
}

fn line_col_to_offset(source: &str, line: usize, col: usize) -> usize {
    let line_start: usize = source
        .lines()
        .take(line.saturating_sub(1))
        .map(|l| l.len() + 1)
        .sum();
    let line_len = source
        .lines()
        .nth(line.saturating_sub(1))
        .map_or(0, |l| l.len());
    line_start + col.saturating_sub(1).min(line_len)
}

fn render_diagnostic(
    source: &str,
    filename: &str,
    line: usize,
    col: usize,
    message: &str,
) -> String {
    let file = SimpleFile::new(filename, source);
    let offset = line_col_to_offset(source, line, col);
    let end = (offset + 1).min(source.len());
    let diag = Diagnostic::error()
        .with_message(message)
        .with_labels(vec![Label::primary((), offset..end)]);
    term::emit_into_string(&Config::default(), &file, &diag)
        .unwrap_or_else(|_| message.to_string())
        .trim_end()
        .to_string()
}
