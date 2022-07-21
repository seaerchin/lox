use crate::error::{self, error, Result};

use super::expr::*;

pub enum LoxValue {
    // NOTE: we use a pointer to a dummy trait object
    // any struct/value that implements this object means that
    // it can be a potential runtime lox value
    Any(Box<dyn DynamicLoxValue>),
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
        LoxValue::Any(val) => {
            if let UnaryOp::Not = op {
                if is_truthy(val) {
                    return Ok(LoxValue::Boolean(false));
                } else {
                    return Ok(LoxValue::Boolean(true));
                }
            } else {
                return Err(error(
                    line,
                    "Unable to apply negate operator to non-numeric type",
                ));
            }
        }
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

fn eval_bin(left_expr: Expr, op: Op, right_expr: Expr) -> Result<LoxValue> {
    let left = eval(left_expr)?;
    // NOTE: This is eagerly evaluated
    let right = eval(right_expr)?;
    match op {
        // if types are different, we can return false
        // else, compare
        Op::EqEq => todo!(),
        // flip above
        Op::NotEq => todo!(),
        // check if numeric else throw error
        Op::Lesser => todo!(),
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
    todo!()
}

// peek at runtime val and return true if it is truthy
// diverges from book - empty strings and 0 is falsey
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
