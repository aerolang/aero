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

    // Compile using the codegen backend
    airc_codegen::compile_air(
        &source,
        args.output.to_str().unwrap()
    );

    Ok(())
}
