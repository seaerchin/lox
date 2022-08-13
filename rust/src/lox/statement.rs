use crate::expr::Expr;

// A statement is either an expression
// or some other effectful statement terminated by ;
pub enum Statement {
    ExprStmt(Expr),
    PrintStmt(Expr),
}
