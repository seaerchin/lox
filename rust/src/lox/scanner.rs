use std::str::FromStr;

use crate::{
    error::{self, DynErr},
    token::{Literal, ScannerError, Token, TokenResult, TokenType},
};

pub type ScannerResult<T> = Result<T, DynErr>;

// store the state into our struct and manipulate it from there
pub struct Scanner<'a> {
    source: &'a str,
    line: u64,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            line: 1,
            start: 0,
            current: 0,
            tokens: vec![],
        }
    }

    // Only advance if there is a character to consume
    // from our input stream
    fn advance(&mut self) -> Option<&str> {
        peek(self.source).map(|c| {
            self.current += 1;
            c
        })
    }

    pub fn scan_token(&mut self) -> TokenResult {
        // should better structure to avoid this extra alloc
        let source = self.source.to_owned();
        let line = self.line;
        match self.advance() {
            // NOTE: need to advance the stream by examining the lexeme
            Some(c) => match_with_look_ahead(0, &source, c, line)
                .or_else(|| match_with_look_ahead(1, &source, c, line))
                .or_else(|| match_with_look_ahead(2, &source, c, line))
                .ok_or_else(|| ScannerError::IncompleteLexeme),
            None => Ok(Token::new(TokenType::EOF, "", Literal::EOF, 0)),
        }
    }
}

// matches a single character to return a token or returns the stream unchanged
fn match_with_look_ahead(
    num_lookahead: u64,
    remaining: &str,
    cur_char: &str,
    cur_line: u64,
) -> Option<Token> {
    look(num_lookahead, remaining).and_then(|extracted_chars| {
        let raw_lexeme = cur_char.to_string().extend(extracted_chars.chars());
        match num_lookahead {
            0 => match_single_char(cur_char, &remaining[1..], cur_line),
            1 => {
                // construct current lexeme with cur_char + peeking
                look(2, remaining).and_then(|s| {
                    let cur_lexeme = format!("{cur_char}{remaining}");
                    match_double(&cur_lexeme, &remaining[2..], cur_line)
                })
            }
            _ => todo!(),
        }
    })
}

fn match_single_char(cur_char: &str, remaining: &str, line: u64) -> Option<Token> {
    let next_char = peek(remaining);
    let x: i32 = -1;
    match cur_char {
        "(" => Some(Token::new(
            TokenType::LEFT_PAREN,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        ")" => Some(Token::new(
            TokenType::RIGHT_PAREN,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "{" => Some(Token::new(
            TokenType::LEFT_BRACE,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "}" => Some(Token::new(
            TokenType::RIGHT_BRACE,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "," => Some(Token::new(
            TokenType::COMMA,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "." => Some(Token::new(
            TokenType::DOT,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "-" => Some(Token::new(
            TokenType::MINUS,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "+" => Some(Token::new(
            TokenType::PLUS,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        ";" => Some(Token::new(
            TokenType::SEMICOLON,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        "*" => Some(Token::new(
            TokenType::STAR,
            cur_char,
            Literal::String(cur_char.to_string()),
            line,
        )),
        // Don't tackle the 2 char section here
        // will fall through to next portion
        "!" => next_char.and_then(|c| {
            if c != "=" {
                Some(Token::new(
                    TokenType::BANG,
                    cur_char,
                    Literal::String(cur_char.to_string()),
                    line,
                ))
            } else {
                None
            }
        }),
        "=" => next_char.and_then(|c| {
            if c != "=" {
                Some(Token::new(
                    TokenType::EQUAL,
                    cur_char,
                    Literal::String(cur_char.to_string()),
                    line,
                ))
            } else {
                None
            }
        }),
        "<" => next_char.and_then(|c| {
            if c != "=" {
                Some(Token::new(
                    TokenType::LESS,
                    cur_char,
                    Literal::String(cur_char.to_string()),
                    line,
                ))
            } else {
                None
            }
        }),
        ">" => next_char.and_then(|c| {
            if c != "=" {
                Some(Token::new(
                    TokenType::GREATER,
                    cur_char,
                    Literal::String(cur_char.to_string()),
                    line,
                ))
            } else {
                None
            }
        }),
        _ => None,
    }
}

fn match_double(lexeme: &str, remaining: &str, line: u64) -> Option<Token> {
    let _ = match lexeme {
        "!=" => Some(Token::new(
            TokenType::BANG_EQUAL,
            "!=",
            Literal::String("!=".to_owned()),
            line,
        )),
        "==" => Some(Token::new(
            TokenType::EQUAL_EQUAL,
            "==",
            Literal::String("==".to_owned()),
            line,
        )),
        "<=" => Some(Token::new(
            TokenType::LESS_EQUAL,
            "<=",
            Literal::String("<=".to_owned()),
            line,
        )),
        ">=" => Some(Token::new(
            TokenType::GREATER_EQUAL,
            ">=",
            Literal::String(">=".to_owned()),
            line,
        )),
        _ => None,
    };
    todo!()
}

// lookahead(1)
fn peek(source: &str) -> Option<&str> {
    look(1, source)
}

fn look(n: u64, source: &str) -> Option<&str> {
    source.get(1..(n + 1) as usize)
}
