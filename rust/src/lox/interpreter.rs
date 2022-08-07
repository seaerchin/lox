use super::expr::*;
use crate::error::{error, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
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

fn eval_grouping(expr: Box<Expr>) -> Result<LoxValue> {
    eval(*expr)
}

fn eval(expr: Expr) -> Result<LoxValue> {
    match expr.expr {
        ExprType::Literal(_) => todo!(),
        ExprType::Grouping(_) => todo!(),
        ExprType::Unary(_, _) => todo!(),
        ExprType::Binary(_, _, _) => todo!(),
    }
    todo!()
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
            _ => Err(error(line, "Unable to apply negate operator to nil")),
        },
        // ^
        Op::LEq => todo!(),
        // ^
        Op::Greater => todo!(),
        // ^
        Op::GEq => todo!(),
        // ^ + for strings, we will append; we will NOT attempt a conversion to string but rather, report an error
        Op::Plus => todo!(),
        // ^
        Op::Minus => todo!(),
        // ^
        Op::Star => todo!(),
        // ^
        Op::Slash => todo!(),
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
