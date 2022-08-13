use std::{fs, io, process::exit};

use crate::{error::DynErr, interpreter::interpret, parser::Parser, scanner::Scanner};

pub fn run_file(file_name: &str) {
    let contents = fs::read_to_string(file_name).expect("Something went wrong reading the file");
    let errors = run(&contents);
    if errors.len() > 0 {
        exit(65);
    }
}

pub fn run_prompt() {
    let stdin = io::stdin();
    loop {
        let mut buf = String::new();
        print!("> ");
        stdin
            .read_line(&mut buf)
            .expect("Something went wrong while attempting to read input from stdin");
        if buf.is_empty() {
            break;
        }
        let _ = run(&buf);
    }
}

// NOTE: We should redesign the API here so that the monadic style
// of doing things becomes easier.
// The API given here is slightly messy and not ideal.
fn run(line: &str) -> Vec<DynErr> {
    // initialize the scanner
    // then we scan tokens
    let scanner = Scanner::new(line);
    let (tokens, mut errors) = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);

    let stmt_results = parser.parse();
    let stmts = stmt_results
        .iter()
        .filter(|res| res.is_ok())
        .map(|ok| (*ok).as_ref().unwrap().to_owned())
        .collect();

    interpret(stmts);

    let mut stmt_errors = stmt_results
        .into_iter()
        .filter(|res| res.is_err())
        .map(|err| err.unwrap_err())
        .collect();

    errors.append(&mut stmt_errors);

    errors
}
