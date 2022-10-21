use std::{
    error::Error,
    io::{Error as IoError, ErrorKind},
    result::Result as StdResult,
};

use crate::token::{Token, TokenType};

pub type DynErr = Box<dyn Error>;
pub type Result<T> = StdResult<T, DynErr>;

pub fn error(line: u64, message: &str) -> DynErr {
    report(line, "", message)
}

fn report(line: u64, where_error: &str, message: &str) -> DynErr {
    let message = format!("\n[line: {line}] Error {where_error}: ${message}\n");
    Box::new(IoError::new(ErrorKind::InvalidData, message))
}

pub fn parse_error(token: &Token, message: &str) -> DynErr {
    if token.token_type == TokenType::EOF {
        report(token.line, " at end", message)
    } else {
        let lex = token.lexeme.as_str();
        report(token.line, &format!(" at '{lex}'"), message)
    }
}
