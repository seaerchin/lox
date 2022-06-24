use std::{
    error::Error,
    io::{stderr, Error as IoError, ErrorKind, Write},
};

pub fn error(line: i32, message: &str) -> Box<dyn Error> {
    report(line, "", message)
}

fn report(line: i32, where_error: &str, message: &str) -> Box<dyn Error> {
    let message = format!("[line: {line}] Error {where_error}: ${message}");
    stderr()
        .write(message.as_bytes())
        .expect("something went wrong while trying to write to stderr");

    Box::new(IoError::new(ErrorKind::InvalidData, message))
}
