use rust::cli;
use rust::lox::*;

fn main() {
    if let Some(file) = cli::get_args() {
        lox::run_file(&file)
    }

    lox::run_prompt();
}
