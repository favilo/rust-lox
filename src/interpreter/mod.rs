use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc, time::UNIX_EPOCH};

use crate::{
    error::EvaluateError,
    parser::{NativeCallable, Value},
};

#[derive(Debug, Clone)]
pub struct Context {
    env: Rc<RefCell<Environment>>,
    stdout: Option<Rc<RefCell<String>>>,
    stack_depth: Rc<RefCell<usize>>,
}

impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.env, &other.env)
            && self.stdout == other.stdout
            && self.stack_depth == other.stack_depth
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
            stdout: None,
            stack_depth: Rc::default(),
        }
    }

    pub fn with_stdout(self) -> Self {
        Self {
            env: Rc::clone(&self.env),
            stdout: Some(Rc::default()),
            stack_depth: self.stack_depth,
        }
    }

    pub fn stack_depth(&self) -> usize {
        *self.stack_depth.borrow()
    }

    pub fn push_stack(&self) {
        *self.stack_depth.borrow_mut() += 1;
    }

    pub fn pop_stack(&self) {
        *self.stack_depth.borrow_mut() -= 1;
    }

    pub fn get(&self, name: &str, depth: Option<usize>) -> Result<Value, EvaluateError> {
        self.env.borrow().get(name, depth)
    }

    pub fn set(&self, name: &str, value: Value, depth: Option<usize>) -> Result<(), EvaluateError> {
        self.env.borrow_mut().set(name, value, depth)
    }

    pub fn declare(
        &self,
        name: &str,
        value: Value,
        depth: Option<usize>,
    ) -> Result<(), EvaluateError> {
        self.env.borrow_mut().declare(name, value, depth)
    }

    pub fn child(&self) -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment {
                stack: HashMap::default(),
                globals: Rc::clone(&self.env.borrow().globals),
                parent: Some(Rc::clone(&self.env)),
            })),
            stdout: self.stdout.clone(),
            stack_depth: self.stack_depth.clone(),
        }
    }

    pub fn depth(&self) -> Option<usize> {
        self.env.borrow().depth()
    }

    pub fn print(&self, value: &Value) {
        if let Some(stdout) = self.stdout.as_ref() {
            let mut stdout = stdout.borrow_mut();
            stdout.push_str(&value.to_string());
            stdout.push('\n');
        } else {
            println!("{value}");
        }
    }

    pub fn stdout(&self) -> Option<String> {
        self.stdout.as_ref().map(|s| s.borrow().clone())
    }

    // pub fn make_clone(&self) -> Self {
    //     let env = self.env.borrow();
    //     Self {
    //         env: Rc::new(RefCell::new(Environment {
    //             parent: Some(Rc::new(RefCell::new(env.clone()))),
    //             stack: Default::default(),
    //             globals: env.globals.clone(),
    //         })),
    //     }
    // }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub(crate) struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    pub(crate) stack: HashMap<String, Value>,
    globals: Rc<RefCell<HashMap<String, Value>>>,
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("Environment");
        builder
            .field("globals", &self.globals)
            .field("stack", &self.stack);
        if let Some(parent) = self.parent.as_ref() {
            builder.field_with("parent", |f| {
                writeln!(f, "[")?;
                parent.borrow().debug_helper(f)?;
                write!(f, "]")
            });
        }
        builder.finish()
    }
}
fn clock_fn(_: &[Value], _: &Context) -> Value {
    Value::from(UNIX_EPOCH.elapsed().unwrap().as_secs_f64())
}

fn debug_value(args: &[Value], env: &Context) -> Value {
    args.iter()
        .for_each(|arg| env.print(&Value::String(format!("{arg:?}").into())));
    Value::Nil
}

impl Environment {
    fn debug_helper(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:#?}", &self.stack)?;
        let Some(parent) = self.parent.as_ref() else {
            return Ok(());
        };
        parent.borrow().debug_helper(f)
    }

    pub(crate) fn new() -> Self {
        let mut globals = HashMap::default();
        globals.insert(
            "clock".into(),
            Value::NativeCallable(NativeCallable {
                arg_num: 0,
                func: Arc::new(clock_fn),
            }),
        );
        globals.insert(
            "debug".into(),
            Value::NativeCallable(NativeCallable {
                arg_num: 1,
                func: Arc::new(debug_value),
            }),
        );
        Self {
            parent: None,
            stack: HashMap::default(),
            globals: Rc::new(RefCell::new(globals)),
        }
    }

    fn get(&self, name: &str, depth: Option<usize>) -> Result<Value, EvaluateError> {
        let Some(depth) = depth else {
            return self
                .globals
                .borrow()
                .get(name)
                .cloned()
                .ok_or_else(|| EvaluateError::UndefinedVariable(name.to_string()));
        };
        if depth == 0 {
            return self
                .stack
                .get(name)
                .cloned()
                .ok_or_else(|| EvaluateError::UndefinedVariable(name.to_string()));
        }
        let parent = self
            .parent
            .as_ref()
            .ok_or_else(|| EvaluateError::UndefinedVariable(name.to_string()))?
            .borrow();
        parent.get(name, Some(depth - 1))
    }

    fn set(&mut self, name: &str, value: Value, depth: Option<usize>) -> Result<(), EvaluateError> {
        let Some(depth) = depth else {
            let mut borrow_mut = self.globals.borrow_mut();
            *borrow_mut
                .get_mut(name)
                .ok_or_else(|| EvaluateError::UndefinedVariable(name.to_string()))? = value;
            return Ok(());
        };
        if depth == 0 {
            *self
                .stack
                .get_mut(name)
                .ok_or_else(|| EvaluateError::UndefinedVariable(name.to_string()))? = value;
            return Ok(());
        }

        let parent = self.parent.as_ref();
        if let Some(parent) = parent {
            parent
                .borrow_mut()
                .set(name, value.clone(), Some(depth - 1))
        } else {
            Err(EvaluateError::UndefinedVariable(name.to_string()))
        }
    }

    fn declare(
        &mut self,
        name: &str,
        value: Value,
        depth: Option<usize>,
    ) -> Result<(), EvaluateError> {
        if depth.is_none() {
            self.globals.borrow_mut().insert(name.to_string(), value);
        } else {
            if self.stack.contains_key(name) {
                return Err(EvaluateError::AlreadyDefined(name.to_string()));
            }
            self.stack.insert(name.to_string(), value);
        }
        Ok(())
    }

    fn depth(&self) -> Option<usize> {
        let parent = self.parent.as_ref()?;
        let borrow = parent.borrow();
        Some(borrow.depth().unwrap_or(1) + 1)
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
