use std::fs;

use clap::{Args, Parser, Subcommand};

use crate::parser::parse;

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build(BuildArgs),
}

#[derive(Args)]
struct BuildArgs {
    filename: String,
}

pub fn run() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build(args) => {
            let input = fs::read_to_string(args.filename).expect("Failed to read input file");

            let source = parse(&input);
            println!("{:#?}", source);
        }
    }
}
