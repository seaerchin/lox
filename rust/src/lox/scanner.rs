use crate::{
    error::{self, DynErr},
    token::{Literal, ScannerError, Token, TokenResult, TokenType},
};

pub type ScannerResult<T> = Result<T, DynErr>;

pub struct Scanner<'a> {
    source: &'a str,
    line: usize,
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
    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.source.chars().nth(self.current) {
            self.current += 1;
            return Some(c);
        }
        None
    }

    pub fn scan_token(&mut self) -> TokenResult {
        match self.advance() {
            // need to advance the stream by examining the lexeme
            Some(c) => match_single_char(self.source, c)
                .or_else(|remaining| match_with_look_ahead(1, remaining, c))
                .or_else(|remaining| match_with_look_ahead(2, remaining, c))
                .map_err(|s| ScannerError::IncompleteLexeme),
            None => Ok(Token::new(TokenType::EOF, "", Literal::EOF, 0)),
        }
    }
}

// matches a single character to return a token or returns the stream unchanged
fn match_single_char(remaining: &str, cur_char: char) -> Result<Token, &str> {
    match_with_look_ahead(0, remaining, cur_char)
}

fn match_with_look_ahead(
    num_lookahead: u64,
    remaining: &str,
    cur_char: char,
) -> Result<Token, &str> {
    todo!()
}
