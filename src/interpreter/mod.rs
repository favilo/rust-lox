use std::collections::HashMap;

use crate::parser::expr::Literal;

#[derive(Debug, Default, Clone)]
pub struct InterpreterState {
    globals: HashMap<String, Literal>,
}

impl InterpreterState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, name: &str) -> Option<&Literal> {
        self.globals.get(name)
    }

    pub fn set(&mut self, name: &str, value: Literal) {
        assert!(!matches!(&value, Literal::Id(_)));

        self.globals.insert(name.to_string(), value);
    }
}
