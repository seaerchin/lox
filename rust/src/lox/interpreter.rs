use std::fmt::Display;

use super::expr::*;
use crate::{
    error::{error, Result},
    statement::Statement,
};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    // NOTE: we are missing a type here for the Any type present in lox
    // as lox is a dynamic language but the host language here is Rust,
    // which is a statically typed language
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "null"),
            LoxValue::Boolean(bool) => {
                write!(f, "{}", bool.to_string())
            }
            LoxValue::Number(num) => {
                write!(f, "{}", num.to_string())
            }
            LoxValue::String(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

// trait marker to signify that the implementing type
// is a dynamic type whose type is only determined at runtime
pub trait DynamicLoxValue {
    fn is_truthy(&self) -> bool;
}

fn eval_lit(expr: RawLiteral) -> Result<LoxValue> {
    match expr {
        RawLiteral::False => Ok(LoxValue::Boolean(false)),
        RawLiteral::Number(num) => Ok(LoxValue::Number(num)),
        RawLiteral::String(s) => Ok(LoxValue::String(s)),
        RawLiteral::True => Ok(LoxValue::Boolean(true)),
        RawLiteral::Nil => Ok(LoxValue::Nil),
    }
}

fn execute(stmt: Statement) {
    match stmt {
        Statement::ExprStmt(expr) => {
            _ = eval(expr);
        }
        Statement::PrintStmt(expr) => {
            _ = eval(expr)
                .and_then(|value| {
                    println!("{}", value.to_string());
                    Ok(())
                })
                .or_else(|err| {
                    print!("{err}");
                    Err(())
                });
        }
    }
}

fn eval(expr: Expr) -> Result<LoxValue> {
    match expr.expr {
        ExprType::Literal(lit) => eval_lit(lit),
        ExprType::Grouping(expr) => eval(*expr),
        ExprType::Unary(op, expr) => eval_unary(op, *expr),
        ExprType::Binary(left, op, right) => eval_bin(*left, op, *right),
    }
}

// NOTE: since we have an operator here,
// we will choose to crash if the type doesn't match
fn eval_unary(op: UnaryOp, expr: Expr) -> Result<LoxValue> {
    let line = expr.token.line;
    let value = eval(expr)?;
    match value {
        LoxValue::Nil => {
            return match op {
                UnaryOp::Neg => Err(error(line, "Unable to apply negate operator to nil")),
                UnaryOp::Not => return Ok(LoxValue::Boolean(true)),
            }
        }
        LoxValue::Boolean(b) => {
            return match op {
                UnaryOp::Neg => Err(error(line, "Unable to apply negate operator to bool")),
                UnaryOp::Not => return Ok(LoxValue::Boolean(!b)),
            }
        }
        LoxValue::Number(n) => {
            return match op {
                UnaryOp::Neg => Ok(LoxValue::Number(-n)),
                UnaryOp::Not => {
                    if is_truthy(Box::new(n)) {
                        Ok(LoxValue::Boolean(false))
                    } else {
                        Ok(LoxValue::Boolean(true))
                    }
                }
            }
        }
        LoxValue::String(s) => {
            return match op {
                UnaryOp::Neg => Err(error(line, "Unable to apply negate operator to string")),
                UnaryOp::Not => {
                    if is_truthy(Box::new(s)) {
                        Ok(LoxValue::Boolean(false))
                    } else {
                        Ok(LoxValue::Boolean(true))
                    }
                }
            }
        }
    }
}

// Refer to types here:
// https://github.com/yanchith/relox/blob/master/src/value.rs
fn eval_bin(left_expr: Expr, op: Op, right_expr: Expr) -> Result<LoxValue> {
    let line = left_expr.token.line;
    let left = eval(left_expr)?;
    // NOTE: This is eagerly evaluated
    let right = eval(right_expr)?;
    match op {
        // if types are different, we can return false
        // else, compare
        Op::EqEq => Ok(LoxValue::Boolean(left == right)),
        // flip above
        Op::NotEq => Ok(LoxValue::Boolean(left != right)),
        // check if numeric else throw error
        Op::Lesser => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => {
                Ok(LoxValue::Boolean(left < right))
            }
            _ => Err(error(line, "Unable to compare non numeric types")),
        },
        // ^
        Op::LEq => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => {
                Ok(LoxValue::Boolean(left <= right))
            }
            _ => Err(error(line, "Unable to compare non numeric types")),
        },
        // ^
        Op::Greater => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => {
                Ok(LoxValue::Boolean(left > right))
            }
            _ => Err(error(line, "Unable to compare non numeric types")),
        },
        // ^
        Op::GEq => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => {
                Ok(LoxValue::Boolean(left >= right))
            }
            _ => Err(error(line, "Unable to compare non numeric types")),
        },
        // ^ + for strings, we will append; we will NOT attempt a conversion to string but rather, report an error
        Op::Plus => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => Ok(LoxValue::Number(left + right)),
            (LoxValue::String(left), LoxValue::String(right)) => {
                Ok(LoxValue::String(left + &right))
            }
            _ => Err(error(
                line,
                "Unable to add non numeric types and non string types",
            )),
        },
        // ^
        Op::Minus => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => Ok(LoxValue::Number(left - right)),
            _ => Err(error(line, "Unable to subtract non numeric types")),
        },
        // ^
        Op::Star => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => Ok(LoxValue::Number(left * right)),
            _ => Err(error(line, "Unable to multiply non numeric types")),
        },
        // ^
        Op::Slash => match (left, right) {
            (LoxValue::Number(left), LoxValue::Number(right)) => Ok(LoxValue::Number(left / right)),
            _ => Err(error(line, "Unable to divide non numeric types")),
        },
    }
}

// peek at runtime val and return true if it is truthy
// diverges from book - empty strings and 0 is falsey
// tbc - this might be more complex for a mvp
fn is_truthy(val: Box<dyn DynamicLoxValue>) -> bool {
    val.is_truthy()
}

impl DynamicLoxValue for f64 {
    fn is_truthy(&self) -> bool {
        if *self == 0.0 {
            return false;
        } else {
            return true;
        }
    }
}

impl DynamicLoxValue for String {
    fn is_truthy(&self) -> bool {
        return self.is_empty();
    }
}

pub fn interpret(stmts: Vec<Statement>) {
    for stmt in stmts {
        execute(stmt)
    }
}
