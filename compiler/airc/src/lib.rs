use std::path::Path;

/// Compile an AIR source string to an executable
///
/// # Arguments
/// * `source` - The AIR source code as a string
/// * `output_path` - Path where the executable should be written
/// * `runtime_path` - Path to the runtime library (libruntime.a)
///
/// # Returns
/// * `Ok(())` on successful compilation
/// * `Err` if parsing or compilation fails
pub fn compile_air(
    source: &str,
    output_path: &str,
    runtime_path: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    // Validate runtime library exists
    if !Path::new(runtime_path).exists() {
        return Err(format!("Runtime library not found at {}", runtime_path).into());
    }

    // Parse the AIR source using the Rust parser
    let ast = airc_syntax::parse_air(source)
        .map_err(|e| format!("Parse error: {}", e))?;

    // Convert AST to FFI format
    let source_data = airc_codegen::convert_ast_to_ffi(&ast);

    // Compile using the AST-based codegen backend
    airc_codegen::compile_air_ast(
        vec![source_data],
        output_path,
        runtime_path,
    );

    Ok(())
}
