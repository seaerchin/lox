use std::{collections::HashMap, error::Error, hash::Hash};

use crate::{
    error,
    token::{Literal, Token, TokenType},
};

pub struct Scanner<'a> {
    source: &'a str,
    line: usize,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
}

pub type DynErr = Box<dyn Error>;
pub type ScannerResult<T> = Result<T, DynErr>;

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            line: 1,
            start: 0,
            current: 0,
            tokens: vec![],
        }
    }

    fn add_token_with_lit(&mut self, token_type: TokenType, lit: Literal) {
        let text = &self.source[self.start..self.current];
        let tok = Token::new(token_type, text, lit, self.line);
        self.tokens.push(tok);
    }

    fn advance(&mut self) -> char {
        let cur_char = self.source.chars().nth(self.current).expect("overflow: tried to index string with invalid index. this could be because the string consists of codepoints consisting of multiple bytes");
        self.current += 1;
        cur_char
    }

    fn is_at_end(&self) -> bool {
        self.source.len() <= self.current
    }

    fn add_token(&mut self, token_type: TokenType, literal: Literal) {
        self.add_token_with_lit(token_type, literal)
    }

    // convenience function so we don't have to keep passing in None
    fn add_token_with_none_lit(&mut self, token_type: TokenType) {
        self.add_token_with_lit(token_type, Literal::None)
    }

    fn scan_token(&mut self) -> ScannerResult<()> {
        // extract single char from underlying stream
        let c = self.advance();

        // construct token from character, taking greedily as needed
        match c {
            // single character tokens
            '(' => self.add_token_with_none_lit(TokenType::LEFT_PAREN),
            ')' => self.add_token_with_none_lit(TokenType::RIGHT_PAREN),
            '{' => self.add_token_with_none_lit(TokenType::LEFT_BRACE),
            '}' => self.add_token_with_none_lit(TokenType::RIGHT_BRACE),
            ',' => self.add_token_with_none_lit(TokenType::COMMA),
            '.' => self.add_token_with_none_lit(TokenType::DOT),
            '-' => self.add_token_with_none_lit(TokenType::MINUS),
            '+' => self.add_token_with_none_lit(TokenType::PLUS),
            ';' => self.add_token_with_none_lit(TokenType::SEMICOLON),
            '*' => self.add_token_with_none_lit(TokenType::STAR),
            // double character tokens -> comparison operators
            '!' => {
                if self.matches('=') {
                    self.add_token_with_none_lit(TokenType::BANG_EQUAL)
                } else {
                    self.add_token_with_none_lit(TokenType::BANG)
                }
            }
            '=' => {
                if self.matches('=') {
                    self.add_token_with_none_lit(TokenType::EQUAL_EQUAL)
                } else {
                    self.add_token_with_none_lit(TokenType::EQUAL_EQUAL)
                }
            }
            '<' => {
                if self.matches('=') {
                    self.add_token_with_none_lit(TokenType::LESS_EQUAL)
                } else {
                    self.add_token_with_none_lit(TokenType::LESS)
                }
            }
            '>' => {
                if self.matches('=') {
                    self.add_token_with_none_lit(TokenType::GREATER_EQUAL)
                } else {
                    self.add_token_with_none_lit(TokenType::GREATER)
                }
            }
            // need to parse to check if it is comment or not
            '/' => {
                // we have a comment - consume everything until we hit EOL
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token_with_none_lit(TokenType::SLASH)
                }
                todo!()
            }
            // ignore whitespace
            ' ' | '\r' | '\t' => (),
            '\n' => {
                self.line += 1;
            }
            '"' => return self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphanumeric() => self.ident(),
            _ => return Err(error::error(self.line, "Unexpected character")),
        }

        Ok(())
    }

    fn ident(&mut self) {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }

        let raw_ident = &self.source[self.start..self.current];
        // check to see if the raw identifier is in our hashmap - otherwise, we know that by the maximal munch rule,
        // it is a user declared identifier even if it starts with a reserved keyword
        let ident = get_ident(raw_ident);
        self.add_token_with_none_lit(ident)
    }

    // we keep taking until the next character isn't a number
    fn number(&mut self) {
        // keep consuming input while the next character isn't a digit
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // we have a decimal number iff next digit is . and the subsequent is still a digit
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // consume the dot
            self.advance();
            self.number()
        }

        let extracted_number: f64 = self.source[self.start..self.current]
            .parse::<f64>()
            .unwrap();

        // when we hit this section, we are guaranteed that either
        // subsection is a number
        // or that we have hit EOL
        self.add_token(TokenType::NUMBER, Literal::Number(extracted_number))
    }

    fn peek_next(&self) -> char {
        if self.current + 1 > self.source.len() {
            return '\0';
        } else {
            self.source.chars().nth(self.current + 1).unwrap()
        }
    }

    fn string(&mut self) -> ScannerResult<()> {
        // consume from input stream until we hit the terminating " character
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        // check why we exit - either we hit closing " or we are at the end of our input
        if self.is_at_end() {
            Err(error::error(self.line, "Unterminated string"))
        } else {
            // consume closing "
            self.advance();
            self.add_token(
                TokenType::STRING,
                // strip leading/terminating quotes
                Literal::String(self.source[self.start + 1..self.current + 1].to_string()),
            );
            Ok(())
        }
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn matches(&self, c: char) -> bool {
        let next_idx = self.current + 1;
        self.source
            .chars()
            .nth(next_idx)
            .map_or(false, |next| next == c)
    }

    pub fn scan_tokens(mut self) -> (Vec<Token>, Vec<DynErr>) {
        let mut errors = vec![];

        while !self.is_at_end() {
            self.start = self.current;
            let err = self.scan_token();
            if err.is_err() {
                errors.push(err.err().unwrap());
            }
        }

        self.tokens
            .push(Token::new(TokenType::EOF, "", Literal::None, self.line));

        (self.tokens, errors)
    }
}

fn get_ident(raw_token: &str) -> TokenType {
    match raw_token {
        "and" => TokenType::AND,
        "class" => TokenType::CLASS,
        "else" => TokenType::ELSE,
        "false" => TokenType::FALSE,
        "for" => TokenType::FOR,
        "fun" => TokenType::FUN,
        "if" => TokenType::IF,
        "nil" => TokenType::NIL,
        "or" => TokenType::OR,
        "print" => TokenType::PRINT,
        "return" => TokenType::RETURN,
        "super" => TokenType::SUPER,
        "this" => TokenType::THIS,
        "true" => TokenType::TRUE,
        "var" => TokenType::VAR,
        "while" => TokenType::WHILE,
        _ => TokenType::IDENTIFIER,
    }
}
