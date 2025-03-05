use std::collections::HashMap;

use crate::parser::expr::Literal;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    environments: Vec<HashMap<String, Literal>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            environments: vec![HashMap::default()],
        }
    }

    pub fn view(&mut self) -> EnvironmentView {
        EnvironmentView { state: self }
    }

    pub fn get(&self, name: &str) -> Option<&Literal> {
        self.environments.iter().rev().find_map(|env| env.get(name))
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        assert!(!matches!(&value, Literal::Id(_)));

        self.environments
            .last_mut()
            .expect("always have one environment")
            .insert(name.to_string(), value);
    }

    pub fn set(&mut self, name: &str, value: Literal) {
        assert!(!matches!(&value, Literal::Id(_)));

        let (last, rest) = self
            .environments
            .split_last_mut()
            .expect("always have one environment");

        let found = if last.contains_key(name) {
            None
        } else {
            rest.iter_mut().rev().find(|env| env.contains_key(name))
        };
        let env = found.unwrap_or(last);
        env.insert(name.to_string(), value);
    }
}

#[derive(Debug)]
pub struct EnvironmentView<'a> {
    state: &'a mut Environment,
}

impl Drop for EnvironmentView<'_> {
    fn drop(&mut self) {
        if self.state.environments.len() > 1 {
            self.state.environments.pop();
        }
    }
}

impl EnvironmentView<'_> {
    pub fn child_view(&mut self) -> EnvironmentView {
        self.state.environments.push(HashMap::default());
        EnvironmentView { state: self.state }
    }

    pub fn get(&self, name: &str) -> Option<&Literal> {
        self.state.get(name)
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.state.define(name, value);
    }

    pub fn set(&mut self, name: &str, value: Literal) {
        self.state.set(name, value);
    }
}
