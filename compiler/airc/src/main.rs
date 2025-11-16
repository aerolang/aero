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

    airc::compile_air(
        &source,
        args.output.to_str().unwrap(),
        runtime_path.to_str().unwrap(),
    )?;

    Ok(())
}
