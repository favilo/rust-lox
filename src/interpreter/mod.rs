use std::{collections::HashMap, sync::Arc, time::UNIX_EPOCH};

use crate::parser::Value;

#[derive(Debug, Default, Clone)]
pub struct Environment {
    environments: Vec<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut globals = HashMap::default();
        globals.insert(
            "clock".into(),
            Value::NativeCallable(
                0,
                Arc::new(|_| Value::from(UNIX_EPOCH.elapsed().unwrap().as_secs_f64())),
            ),
        );
        Self {
            environments: vec![globals],
        }
    }

    pub fn view(&mut self) -> EnvironmentView {
        EnvironmentView { state: self }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.environments.iter().rev().find_map(|env| env.get(name))
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.environments
            .last_mut()
            .expect("always have one environment")
            .insert(name.to_string(), value);
    }

    pub fn set(&mut self, name: &str, value: Value) {
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

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.state.get(name)
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.state.define(name, value);
    }

    pub fn set(&mut self, name: &str, value: Value) {
        self.state.set(name, value);
    }
}
