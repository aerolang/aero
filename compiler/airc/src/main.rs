use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "airc")]
#[command(about = "AIR (Aero Intermediate Representation) Compiler", long_about = None)]
struct Args {
    /// Input AIR file
    input: PathBuf,

    /// Output executable path
    #[arg(short, long, default_value = "a.out")]
    output: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Read the AIR source file
    let source = std::fs::read_to_string(&args.input)?;

    println!("Compiling {} to {}", args.input.display(), args.output.display());

    // Find the runtime library relative to the binary
    let exe_path = std::env::current_exe()?;
    let exe_dir = exe_path.parent().ok_or("Failed to get executable directory")?;
    let runtime_path = exe_dir.join("runtime/libruntime.a");

    if !runtime_path.exists() {
        eprintln!("Error: Runtime library not found at {}", runtime_path.display());
        std::process::exit(1);
    }

    // Parse the AIR source using the Rust parser
    let ast = match airc_syntax::parse_air(&source) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    // Convert AST to FFI format
    let source_data = airc_codegen::convert_ast_to_ffi(&ast);

    println!("Found {} function(s)", source_data.defs.len());
    for def in &source_data.defs {
        println!("  - {} (pub: {})", def.name, def.is_pub);
    }

    // Compile using the new AST-based codegen backend
    // Pass the source as a single-element vector
    airc_codegen::compile_air_ast(
        vec![source_data],
        args.output.to_str().unwrap(),
        runtime_path.to_str().unwrap()
    );

    Ok(())
}
