use std::{fs, io, process::exit};

use crate::{
    scanner::{DynErr, Scanner},
    token::Token,
};

pub fn run_file(file_name: &str) {
    let contents = fs::read_to_string(file_name).expect("Something went wrong reading the file");
    let (_, errors) = run(&contents);
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

fn run(line: &str) -> (Vec<Token>, Vec<DynErr>) {
    // initialize the scanner
    // then we scan tokens

    let scanner = Scanner::new(line);
    let (tokens, errors) = scanner.scan_tokens();

    for token in tokens.iter() {
        println!("{:?}", token)
    }

    (tokens, errors)
}
