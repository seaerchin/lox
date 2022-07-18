use core::panic;

use crate::{
    error::{parse_error, DynErr},
    expr::{Expr, ExprType, Op, RawLiteral, UnaryOp},
    token::{Literal, Token, TokenType},
};

// we will write this in a functional style
// where Parser<T> hides some state within

pub struct Parser {
    source: Vec<Token>,
    cur: u32,
}

type ParserResult<T> = Result<T, DynErr>;

impl Parser {
    pub fn new(source: Vec<Token>) -> Self {
        Parser { source, cur: 0 }
    }

    pub fn parse(&mut self) -> ParserResult<Expr> {
        return self.expr();
    }

    fn expr(&mut self) -> ParserResult<Expr> {
        self.eq()
    }

    // TODO: simplify all this code
    // they only differ in matches and subsequent method called ._.
    // this would also be alot easier if we could just pass in a function
    fn eq(&mut self) -> ParserResult<Expr> {
        self.comparison().and_then(|left| {
            let eq_op = self
                .extract(TokenType::BANG_EQUAL)
                .or(self.extract(TokenType::EQUAL_EQUAL));

            // we can only terminate once there is no comparison operator matched
            // otherwise, we have to traverse the structure recursively until we hit the above condition
            match eq_op {
                Some(token) => {
                    let right = self.eq().map(|right| {
                        let op_expr = if token.token_type == TokenType::BANG_EQUAL {
                            Op::NotEq
                        } else {
                            Op::EqEq
                        };
                        let bin_expr = ExprType::Binary(Box::new(left), op_expr, Box::new(right));
                        Expr::new(bin_expr, token)
                    });
                    return right;
                }
                None => return Ok(left),
            }
        })
    }

    fn comparison(&mut self) -> ParserResult<Expr> {
        self.term().and_then(|left| {
            // This is horrendously written but we basically keep trying until we get something that succeeds...
            let comp_op = self
                .extract(TokenType::GREATER)
                .or(self.extract(TokenType::GREATER_EQUAL))
                .or(self.extract(TokenType::LESS))
                .or(self.extract(TokenType::LESS_EQUAL));
            match comp_op {
                Some(token) => self.eq().and_then(|right| {
                    let comp_expr = convert_op(token.clone());
                    let bin_expr = ExprType::Binary(Box::new(left), comp_expr, Box::new(right));
                    Ok(Expr::new(bin_expr, token))
                }),
                None => Ok(left),
            }
        })
    }

    fn term(&mut self) -> ParserResult<Expr> {
        let left = self.factor();
        if let Ok(left) = left {
            let factor_op = self
                .extract(TokenType::PLUS)
                .or(self.extract(TokenType::MINUS));

            match factor_op {
                Some(token) => {
                    let right = self.factor();
                    if let Ok(right) = right {
                        let factor_expr = convert_op(token.clone());
                        let bin_expr =
                            ExprType::Binary(Box::new(left), factor_expr, Box::new(right));
                        return Ok(Expr::new(bin_expr, token));
                    }
                    return right;
                }
                None => return Ok(left),
            }
        }
        return left;
    }

    fn factor(&mut self) -> ParserResult<Expr> {
        let left = self.unary();
        if let Ok(left) = left {
            let factor_op = self
                .extract(TokenType::SLASH)
                .or(self.extract(TokenType::STAR));
            match factor_op {
                Some(token) => {
                    let right = self.unary();
                    let factor_expr = convert_op(token.clone());
                    if let Ok(right) = right {
                        let bin_expr =
                            ExprType::Binary(Box::new(left), factor_expr, Box::new(right));
                        return Ok(Expr::new(bin_expr, token));
                    }
                    // we encountered error on right side but not on left
                    return right;
                }
                None => return Ok(left),
            }
        }
        return left;
    }

    fn unary(&mut self) -> ParserResult<Expr> {
        let unary_factor = self
            .extract(TokenType::BANG)
            .or(self.extract(TokenType::MINUS));
        match unary_factor {
            Some(token) => {
                let unary = convert_unary_op(token.clone());
                let expr = self.unary();
                if let Ok(expr) = expr {
                    return Ok(Expr::new(ExprType::Unary(unary, Box::new(expr)), token));
                }
                // error branch; just propagate upwards
                return expr;
            }
            None => self.primary(),
        }
    }

    fn primary(&mut self) -> ParserResult<Expr> {
        let peeked = self.peek().map(|x| x.clone()).unwrap_or(Token::new(
            TokenType::EOF,
            "",
            Literal::None,
            // if we hit EOF, the current token must be valid
            self.source[self.cur as usize].line,
        ));
        let matched = self
            .extract(TokenType::NUMBER)
            .or(self.extract(TokenType::STRING))
            .or(self.extract(TokenType::TRUE))
            .or(self.extract(TokenType::FALSE))
            .or(self.extract(TokenType::NIL))
            .or(self.extract(TokenType::LEFT_PAREN))
            .or(self.extract(TokenType::EOF))
            .unwrap_or_else(|| {
                parse_error(
                    &peeked,
                    &format!("Expect expression at {} but got {:#?}", self.cur, peeked),
                )
                .to_string();
                peeked
            });

        match matched.token_type {
            TokenType::LEFT_PAREN => {
                let expr = self.expr();
                if let Some(_) = self.extract(TokenType::RIGHT_PAREN) {
                    return expr;
                }
                Err(parse_error(
                    &matched,
                    "Expected to find a right parenthesis to match the opening parenthesis",
                ))
            }
            TokenType::STRING => {
                if let Literal::String(s) = &matched.literal {
                    Ok(Expr::new(
                        ExprType::Literal(RawLiteral::String(s.to_owned())),
                        matched,
                    ))
                } else {
                    // impossible case
                    panic!()
                }
            }
            TokenType::NUMBER => {
                if let Literal::Number(n) = matched.literal {
                    Ok(Expr::new(ExprType::Literal(RawLiteral::Number(n)), matched))
                } else {
                    // impossible case
                    panic!()
                }
            }
            TokenType::FALSE => Ok(Expr::new(ExprType::Literal(RawLiteral::False), matched)),
            TokenType::NIL => Ok(Expr::new(ExprType::Literal(RawLiteral::Nil), matched)),
            TokenType::TRUE => Ok(Expr::new(ExprType::Literal(RawLiteral::True), matched)),
            TokenType::EOF => Ok(Expr::new(
                ExprType::Literal(RawLiteral::String("EOF".to_string())),
                matched,
            )),
            _ => panic!(),
        }
    }

    fn extract(&mut self, tok_type: TokenType) -> Option<Token> {
        if let Some(cur) = self.source.get((self.cur) as usize).map(|x| x.clone()) {
            if cur.token_type == tok_type {
                self.cur += 1;
                return Some(cur);
            } else {
                return None;
            }
        }
        None
    }

    fn peek(&self) -> Option<&Token> {
        return self.source.get((self.cur + 1) as usize);
    }

    fn advance(&mut self) -> Option<Token> {
        if self.peek().is_some() {
            self.cur += 1;
            return Some(self.previous());
        }
        None
    }

    fn previous(&self) -> Token {
        self.source[(self.cur - 1) as usize].clone()
    }

    fn synchronize(&mut self) {
        self.advance();

        while self.peek().is_some() {
            if self.previous().token_type == TokenType::SEMICOLON {
                return;
            }

            let next_token = self.peek().unwrap().token_type;

            if next_token == TokenType::CLASS
                || next_token == TokenType::FUN
                || next_token == TokenType::VAR
                || next_token == TokenType::FOR
                || next_token == TokenType::IF
                || next_token == TokenType::WHILE
                || next_token == TokenType::PRINT
                || next_token == TokenType::RETURN
            {
                return;
            }
            self.advance();
        }
    }
}

// should have structured this better but the book uses an extremely oop style
// so we don't have pattern matching across this domain sadly
fn convert_op(token: Token) -> Op {
    match token.token_type {
        TokenType::MINUS => Op::Minus,
        TokenType::PLUS => Op::Plus,
        TokenType::SLASH => Op::Slash,
        TokenType::STAR => Op::Star,
        TokenType::BANG_EQUAL => Op::NotEq,
        TokenType::EQUAL_EQUAL => Op::EqEq,
        TokenType::GREATER => Op::Greater,
        TokenType::GREATER_EQUAL => Op::GEq,
        TokenType::LESS => Op::Lesser,
        TokenType::LESS_EQUAL => Op::LEq,
        _ => todo!(),
    }
}

fn convert_unary_op(token: Token) -> UnaryOp {
    match token.token_type {
        TokenType::MINUS => UnaryOp::Neg,
        TokenType::BANG => UnaryOp::Not,
        _ => todo!(),
    }
}
