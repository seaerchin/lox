use std::str::FromStr;

use crate::{
    error::{self, DynErr},
    token::{Literal, ScannerError, Token, TokenResult, TokenType},
};

// store the state into our struct and manipulate it from there
pub struct ScannerState<'a> {
    source: &'a str,
    line: u64,
    current: &'a str,
    tokens: Vec<Token>,
}

// A scanner is some inner state and an external type
pub struct Scanner<'a, T> {
    state: ScannerState<'a>,
    // option as we could always hit the end of the source
    output: Option<T>,
}

impl<'a> ScannerState<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            line: 1,
            current: "",
            tokens: vec![],
        }
    }

    pub fn to_owned(&self) -> Self {
        Self {
            source: self.source,
            line: self.line,
            current: self.current,
            tokens: self.tokens,
        }
    }

    // advances the stream by num chars if possible
    // and stores the token in current
    // does a full copy at present less for source
    // note that because this is **assumed**
    // to be called after an option unwrap,
    // we will not repeat the bounds check here.
    pub fn forward_by(&self, num: u64) -> Self {
        let to_update = self.source.get(0..num as usize).expect("Invariant violated: expected bounds check to be performed before calling `forward_by`!");
        let remaining = self.source.get((num + 1) as usize..).expect("Invariant violated: expected bounds check to be performed before calling `forward_by`!");
        let current = self.current;
        // alloc but whatever
        let updated = format!("{current}{to_update}");
        Self {
            source: remaining,
            current,
            line: self.line,
            tokens: self.tokens,
        }
    }
}

// Only advance if there is a character to consume
// from our input stream
fn advance<'a>(scanner: &Scanner<'a, &str>) -> Scanner<'a, &'a str> {
    // Peek to see if we can advance
    // and if we can, advance the state.
    // Otherwise, return old state + none
    if let Some(c) = peek(scanner.state.source) {
        let new_state = scanner.state.forward_by(1);
        Scanner {
            state: new_state,
            output: Some(c),
        }
    } else {
        Scanner {
            state: scanner.state.to_owned(),
            output: None,
        }
    }
}

// lookahead(1)
fn peek(source: &str) -> Option<&str> {
    look(1, source)
}

// looks n chars ahead for the source
fn look(n: u64, source: &str) -> Option<&str> {
    source.get(1..(n + 1) as usize)
}

impl<'a, T> Scanner<'a, T> {
    pub fn default(state: ScannerState<'a>) -> Self {
        Self {
            state,
            output: None,
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
                0 => Self::match_single_char(cur_char, &remaining[1..], cur_line),
                1 => {
                    // construct current lexeme with cur_char + peeking
                    look(2, remaining).and_then(|s| {
                        let cur_lexeme = format!("{cur_char}{remaining}");
                        Self::match_double(&cur_lexeme, &remaining[2..], cur_line)
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
            "/" => next_char.and_then(|c| {
                if c != "/" {
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

    fn match_comment() {
        todo!()
    }
}

pub fn scan_token<'a>(scanner: &Scanner<'a, &str>) -> Scanner<'a, &'a str> {
    // should better structure to avoid this extra alloc
    let source = scanner.state.source.to_owned();
    let line = scanner.state.line;
    match advance(scanner).output {
        // NOTE: need to advance the stream by examining the lexeme
        Some(c) => scanner
            .match_with_look_ahead(0, &source, c, line)
            .or_else(|| scanner.match_with_look_ahead(1, &source, c, line))
            .or_else(|| scanner.match_with_look_ahead(2, &source, c, line))
            .ok_or_else(|| ScannerError::IncompleteLexeme),
        None => Ok(Token::new(TokenType::EOF, "", Literal::EOF, 0)),
    }
}
