use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc, time::UNIX_EPOCH};

use crate::parser::Value;

#[derive(Debug, Clone)]
pub struct Context {
    env: Rc<RefCell<Environment>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.env.borrow().get(name)
    }

    pub fn set(&self, name: &str, value: Value) {
        self.env.borrow_mut().set(name, value);
    }

    pub fn declare(&self, name: &str, value: Value) {
        self.env.borrow_mut().declare(name, value)
    }

    pub fn child(&self) -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment {
                parent: Some(Rc::clone(&self.env)),
                environment: Default::default(),
            })),
        }
    }

    pub fn make_clone(&self) -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment {
                parent: Some(Rc::new(RefCell::new(self.env.borrow().clone()))),
                environment: Default::default(),
            })),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    environment: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        let mut environment = HashMap::default();
        environment.insert(
            "clock".into(),
            Value::NativeCallable(
                0,
                Arc::new(|_| Value::from(UNIX_EPOCH.elapsed().unwrap().as_secs_f64())),
            ),
        );
        Self {
            parent: None,
            environment,
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.environment.get(name) {
            return Some(v.clone());
        }
        let parent = self.parent.as_ref()?.borrow();
        parent.get(name)
    }

    fn set(&mut self, name: &str, value: Value) -> bool {
        if self.environment.contains_key(name) {
            self.environment.insert(name.to_string(), value);
            return true;
        }

        let parent = self.parent.as_ref();
        if let Some(parent) = parent {
            if parent.borrow_mut().set(name, value.clone()) {
                return true;
            }
        }
        self.environment.insert(name.to_string(), value);
        true
    }

    fn declare(&mut self, name: &str, value: Value) {
        self.environment.insert(name.to_string(), value);
    }

    // fn pairs<'s>(&'s self) -> Vec<(String, Value)> {
    //     let iter = self.environment.iter().cloned().collect::<Vec<_>>();
    //     let Some(parent) = self.parent.as_ref() else {
    //         return iter;
    //     };

    //     let Some(parent) = parent.read().ok() else {
    //         return iter;
    //     };

    //     iter.into_iter().chain(parent.pairs()).collect()
    // }
}
