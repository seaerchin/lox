use std::{
    error::Error,
    io::{stderr, Error as IoError, ErrorKind, Write},
};

use crate::token::{Token, TokenType};

pub fn error(line: usize, message: &str) -> Box<dyn Error> {
    report(line, "", message)
}

fn report(line: usize, where_error: &str, message: &str) -> Box<dyn Error> {
    let message = format!("\n[line: {line}] Error {where_error}: ${message}\n");
    stderr()
        .write(message.as_bytes())
        .expect("something went wrong while trying to write to stderr");

    Box::new(IoError::new(ErrorKind::InvalidData, message))
}

pub fn parse_error(token: Token, message: &str) -> Box<dyn Error> {
    if token.token_type == TokenType::EOF {
        report(token.line, " at end", message)
    } else {
        let lex = token.lexeme.as_str();
        report(token.line, &format!(" at '{lex}'"), message)
    }
}
