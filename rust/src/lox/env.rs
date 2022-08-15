use std::collections::HashMap;

use crate::{interpreter::LoxValue, token::Token};

pub struct Env(HashMap<String, LoxValue>);

impl Env {
    pub fn new() -> Self {
        Env(HashMap::new())
    }

    // NOTE: This returns the old value prior to update if one already exists;
    // otherwise, this returns None
    pub fn define(&mut self, ident: String, value: LoxValue) -> Option<LoxValue> {
        self.0.insert(ident, value)
    }

    pub fn get(&self, tok: &Token) -> Option<&LoxValue> {
        self.0.get(&tok.lexeme)
    }
}
