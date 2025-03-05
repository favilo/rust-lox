use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};
use interpreter::Environment;

use crate::parser::{Evaluate, Run};

mod error;
mod interpreter;
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
    Run(Common),
}

#[derive(Debug, clap::Args)]
struct Common {
    file_name: PathBuf,
}

fn main() {
    env_logger::init();
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
            let res = parser::expr::parse(&file_contents);
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
            let res = parser::expr::parse(&file_contents);
            let Ok(expr) = res else {
                eprintln!("Failed to parse file {}", res.unwrap_err());
                std::process::exit(65);
            };
            let mut env = Environment::new();
            let mut view = env.view();
            let res = expr.evaluate(&mut view);
            let Ok(result) = res else {
                eprintln!("Failed to evaluate file {}", res.unwrap_err());
                std::process::exit(70);
            };
            println!("{}", result);
        }
        Command::Run(command) => {
            let filename = &command.file_name;
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename.display());
                String::new()
            });
            let res = parser::ast::parse(&file_contents);
            let Ok(ast) = res else {
                eprintln!("Failed to parse file {}", res.unwrap_err());
                std::process::exit(65);
            };
            let res = ast.run();
            if res.is_err() {
                eprintln!("Failed to run file {}", res.unwrap_err());
                std::process::exit(70);
            };
        }
    }
}
