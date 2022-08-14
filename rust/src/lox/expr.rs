use std::fmt::Display;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Literal(RawLiteral),
    Grouping(Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, Op, Box<Expr>),
    Variable(Token),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub expr: ExprType,
    pub token: Token,
}

impl Expr {
    pub fn new(expr: ExprType, token: Token) -> Self {
        Expr { expr, token }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawLiteral {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Op {
    EqEq,
    NotEq,
    Lesser,
    LEq,
    Greater,
    GEq,
    Plus,
    Minus,
    Star,
    Slash,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

impl Display for RawLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawLiteral::Number(raw) => write!(f, "{}", raw.to_string()),
            RawLiteral::String(raw) => write!(f, "{}", raw),
            RawLiteral::True => write!(f, "true"),
            RawLiteral::False => write!(f, "false"),
            RawLiteral::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::EqEq => write!(f, "=="),
            Op::NotEq => write!(f, "!="),
            Op::Lesser => write!(f, "<"),
            Op::LEq => write!(f, "<="),
            Op::Greater => write!(f, ">"),
            Op::GEq => write!(f, ">="),
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Star => write!(f, "*"),
            Op::Slash => write!(f, "/"),
        }
    }
}
