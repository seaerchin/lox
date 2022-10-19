use std::fmt::Display;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
// A literal can only exist as one of a few different variants
pub enum Literal {
    Number(f64),
    String(String),
    None,
    EOF,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lit = match self {
            Literal::Number(n) => n.to_string(),
            Literal::String(s) => s.clone(),
            Literal::None => "None".to_string(),
            Literal::EOF => "EOF".to_string(),
        };

        write!(f, "{lit}")
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TokenType {
    // takes source, literal, line
    // Single character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    //  one or more character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // literals
    IDENTIFIER,
    STRING,
    NUMBER,
    // keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Literal,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: &str, literal: Literal, line: usize) -> Self {
        Token {
            token_type,
            lexeme: lexeme.to_owned(),
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        let tt = &self.token_type;
        let lex = &self.lexeme;
        let lit = &self.literal;
        return format!("{}: {:#?} {lex} {:#?}", self.line, tt, lit);
    }
}

// tbd - a list of scanner errors
pub enum ScannerError {
    IncompleteLexeme,
}

pub type TokenResult = Result<Token, ScannerError>;
