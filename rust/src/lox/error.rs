use std::{
    error::Error,
    io::{Error as IoError, ErrorKind},
};

use crate::token::{Token, TokenType};

pub type DynErr = Box<dyn Error>;

pub fn error(line: usize, message: &str) -> Box<dyn Error> {
    report(line, "", message)
}

fn report(line: usize, where_error: &str, message: &str) -> Box<dyn Error> {
    let message = format!("\n[line: {line}] Error {where_error}: ${message}\n");
    Box::new(IoError::new(ErrorKind::InvalidData, message))
}

pub fn parse_error(token: &Token, message: &str) -> Box<dyn Error> {
    if token.token_type == TokenType::EOF {
        report(token.line, " at end", message)
    } else {
        let lex = token.lexeme.as_str();
        report(token.line, &format!(" at '{lex}'"), message)
    }
}
