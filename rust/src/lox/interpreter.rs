use std::fmt::Display;

use super::expr::*;
use crate::{
    env::Env,
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

fn execute(env: &mut Env, stmt: Statement) {
    match stmt {
        Statement::ExprStmt(expr) => {
            _ = eval(env, expr);
        }
        Statement::PrintStmt(expr) => {
            _ = eval(env, expr)
                .and_then(|value| {
                    println!("{}", value.to_string());
                    Ok(())
                })
                .or_else(|err| {
                    print!("{err}");
                    Err(())
                });
        }
        Statement::Var(ident, expr) => match expr.expr {
            // If the literal matched is nil, we don't evaluate
            // as it might blow up
            ExprType::Literal(RawLiteral::Nil) => {
                env.define(ident.lexeme, LoxValue::Nil);
            }
            _ => {
                // Just define as a side effect and omit the result
                // if we fail the initial eval, we still don't do anything
                let _ = eval(env, expr)
                    .and_then(|value| {
                        env.define(ident.lexeme.clone(), value);
                        Ok(())
                    })
                    // ignore the error - just put null into the env
                    .map_err(|_| {
                        env.define(ident.lexeme, LoxValue::Nil);
                        ()
                    });
            }
        },
    }
}

fn eval(env: &mut Env, expr: Expr) -> Result<LoxValue> {
    match expr.expr {
        ExprType::Literal(lit) => eval_lit(lit),
        ExprType::Grouping(expr) => eval(env, *expr),
        ExprType::Unary(op, expr) => eval_unary(env, op, *expr),
        ExprType::Binary(left, op, right) => eval_bin(env, *left, op, *right),
        // The token given in a variable always refers to the identifier
        ExprType::Variable(ident) => env.get(&ident).map(|val| val.to_owned()).ok_or(error(
            ident.line,
            "Expected value to exist in environment but none was found",
        )),
    }
}

// NOTE: since we have an operator here,
// we will choose to crash if the type doesn't match
fn eval_unary(env: &mut Env, op: UnaryOp, expr: Expr) -> Result<LoxValue> {
    let line = expr.token.line;
    let value = eval(env, expr)?;
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
fn eval_bin(env: &mut Env, left_expr: Expr, op: Op, right_expr: Expr) -> Result<LoxValue> {
    let line = left_expr.token.line;
    let left = eval(env, left_expr)?;
    // NOTE: This is eagerly evaluated
    let right = eval(env, right_expr)?;
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

// NOTE: The abstraction that we truly want here
// is a monadic API (the state monad is ideal here)
// for our environment so the underlying methods (var decl)
// don't have to thread env through but oh well.
pub fn interpret(env: &mut Env, stmts: Vec<Statement>) {
    for stmt in stmts {
        execute(env, stmt)
    }
}
