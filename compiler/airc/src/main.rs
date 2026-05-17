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

fn main() {
    let args = Args::parse();

    if let Err(e) = run(args) {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

fn run(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(&args.input)?;

    println!(
        "Compiling {} to {}",
        args.input.display(),
        args.output.display()
    );

    let exe_path = std::env::current_exe()?;
    let exe_dir = exe_path
        .parent()
        .ok_or("Failed to get executable directory")?;
    let support_path = exe_dir.join("support");

    airc::compile_air(
        &source,
        args.input.to_str().unwrap_or("<input>"),
        args.output.to_str().ok_or("output path is not valid UTF-8")?,
        support_path.to_str().ok_or("support path is not valid UTF-8")?,
    )
}
