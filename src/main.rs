use std::{fs, path::PathBuf};

use ast::Ast;
use clap::{Parser, Subcommand};

mod ast;
mod error;
mod parser;
mod token;

#[derive(Debug, Parser)]
struct Args {
    #[clap(subcommand)]
    cmd: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Tokenize(Common),
    Parse(Common),
    Evaluate(Common),
}

#[derive(Debug, clap::Args)]
struct Common {
    file_name: PathBuf,
}

fn main() {
    let args = Args::parse();

    let command = &args.cmd;

    match command {
        Command::Tokenize(command) => {
            let filename = &command.file_name;
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename.display());
                String::new()
            });
            let res = token::tokenize(&file_contents);
            if res.is_err() {
                std::process::exit(65);
            }
        }
        Command::Parse(command) => {
            let filename = &command.file_name;
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename.display());
                String::new()
            });
            let res = parser::parse(&file_contents);
            let Ok(expr) = res else {
                eprintln!("Failed to parse file {}", res.unwrap_err());
                std::process::exit(65);
            };
            println!("{}", expr);
        }
        Command::Evaluate(command) => {
            let filename = &command.file_name;
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename.display());
                String::new()
            });
            let res = parser::parse(&file_contents);
            let Ok(expr) = res else {
                eprintln!("Failed to parse file {}", res.unwrap_err());
                std::process::exit(65);
            };
            let ast = Ast::new(expr);
            let res = ast.evaluate();
            let Ok(result) = res else {
                eprintln!("Failed to evaluate file {}", res.unwrap_err());
                std::process::exit(65);
            };
            println!("{}", result);
        }
    }
}
