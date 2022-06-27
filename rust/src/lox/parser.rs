use core::panic;

use crate::{
    error::parse_error,
    expr::{Expr, ExprType, Op, RawLiteral, UnaryOp},
    token::{Literal, Token, TokenType},
};

// we will write this in a functional style
// where Parser<T> hides some state within

pub struct Parser {
    source: Vec<Token>,
    cur: u32,
}

impl Parser {
    pub fn new(source: Vec<Token>) -> Self {
        Parser { source, cur: 0 }
    }

    pub fn parse(&mut self) -> Expr {
        return self.expr();
    }

    fn expr(&mut self) -> Expr {
        self.eq()
    }

    // TODO: simplify all this code
    // they only differ in matches and subsequent method called ._.
    // this would also be alot easier if we could just pass in a function
    fn eq(&mut self) -> Expr {
        let left = self.comparison();
        let eq_op = self
            .extract(TokenType::BANG_EQUAL)
            .or(self.extract(TokenType::EQUAL_EQUAL));
        // we can only terminate once there is no comparison operator matched
        // otherwise, we have to traverse the structure recursively until we hit the above condition
        match eq_op {
            Some(token) => {
                let right = self.eq();
                let op_expr = if token.token_type == TokenType::BANG_EQUAL {
                    Op::NotEq
                } else {
                    Op::EqEq
                };
                let bin_expr = ExprType::Binary(Box::new(left), op_expr, Box::new(right));
                Expr::new(bin_expr, token)
            }
            None => left,
        }
    }

    fn comparison(&mut self) -> Expr {
        let left = self.term();
        // This is horrendously written but we basically keep trying until we get something that succeeds...
        let comp_op = self
            .extract(TokenType::GREATER)
            .or(self.extract(TokenType::GREATER_EQUAL))
            .or(self.extract(TokenType::LESS))
            .or(self.extract(TokenType::LESS_EQUAL));
        match comp_op {
            Some(token) => {
                let right = self.eq();
                let comp_expr = convert_op(token.clone());
                let bin_expr = ExprType::Binary(Box::new(left), comp_expr, Box::new(right));
                Expr::new(bin_expr, token)
            }
            None => left,
        }
    }

    fn term(&mut self) -> Expr {
        let left = self.factor();
        let factor_op = self
            .extract(TokenType::PLUS)
            .or(self.extract(TokenType::MINUS));
        match factor_op {
            Some(token) => {
                let right = self.factor();
                let factor_expr = convert_op(token.clone());
                let bin_expr = ExprType::Binary(Box::new(left), factor_expr, Box::new(right));
                Expr::new(bin_expr, token)
            }
            None => left,
        }
    }

    fn factor(&mut self) -> Expr {
        let left = self.unary();
        let factor_op = self
            .extract(TokenType::SLASH)
            .or(self.extract(TokenType::STAR));
        match factor_op {
            Some(token) => {
                let right = self.unary();
                let factor_expr = convert_op(token.clone());
                let bin_expr = ExprType::Binary(Box::new(left), factor_expr, Box::new(right));
                Expr::new(bin_expr, token)
            }
            None => left,
        }
    }

    fn unary(&mut self) -> Expr {
        let unary_factor = self
            .extract(TokenType::BANG)
            .or(self.extract(TokenType::MINUS));
        match unary_factor {
            Some(token) => {
                let unary = convert_unary_op(token.clone());
                let expr = self.unary();
                Expr::new(ExprType::Unary(unary, Box::new(expr)), token)
            }
            None => self.primary(),
        }
    }

    fn primary(&mut self) -> Expr {
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
            .expect(&parse_error(peeked, "Expect expression").to_string());

        match matched.token_type {
            TokenType::LEFT_PAREN => {
                let expr = self.expr();
                self.extract(TokenType::RIGHT_PAREN).expect(
                    "Expected to find a right parenthesis to match the opening parenthesis",
                );
                expr
            }
            TokenType::STRING => {
                if let Literal::String(s) = &matched.literal {
                    Expr::new(ExprType::Literal(RawLiteral::String(s.to_owned())), matched)
                } else {
                    // impossible case
                    panic!()
                }
            }
            TokenType::NUMBER => {
                if let Literal::Number(n) = matched.literal {
                    Expr::new(ExprType::Literal(RawLiteral::Number(n)), matched)
                } else {
                    // impossible case
                    panic!()
                }
            }
            TokenType::FALSE => Expr::new(ExprType::Literal(RawLiteral::False), matched),
            TokenType::NIL => Expr::new(ExprType::Literal(RawLiteral::Nil), matched),
            TokenType::TRUE => Expr::new(ExprType::Literal(RawLiteral::True), matched),
            _ => panic!(),
        }
    }

    fn extract(&mut self, tokType: TokenType) -> Option<Token> {
        let cur = self.source.get((self.cur + 1) as usize).unwrap().clone();
        if cur.token_type == tokType {
            self.advance();
            Some(cur)
        } else {
            None
        }
    }

    fn advance(&mut self) {
        self.cur += 1
    }

    fn peek(&self) -> Option<&Token> {
        return self.source.get((self.cur + 1) as usize);
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
