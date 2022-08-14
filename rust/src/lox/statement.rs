use crate::{expr::Expr, token::Token};

// A statement is either an expression
// or some other effectful statement terminated by ;
#[derive(Debug, Clone)]
pub enum Statement {
    ExprStmt(Expr),
    PrintStmt(Expr),
    Var(Token, Expr),
}
