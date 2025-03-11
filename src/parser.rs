use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

use ast::Statement;
use expr::FnCall;
use winnow::{
    ModalResult, Parser,
    ascii::alpha1,
    combinator::{alt, delimited, eof, opt, preceded, repeat, repeat_till, trace},
    error::ErrMode,
    stream::Stream,
    token::{any, one_of, take_till},
};

use crate::{
    error::{Error, EvaluateError, ParseError, ParseErrorType},
    interpreter::Context,
    parser::state::Stateful,
};

pub mod ast;
pub mod expr;
pub mod state;

pub type InputStream<'s> = &'s str;
pub type Input<'s> = Stateful<InputStream<'s>>;

pub trait Evaluate {
    fn evaluate<'s, 'ctx>(&'s self, _: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx;
}

pub trait Run {
    fn run(&self) -> Result<(), EvaluateError>;
}

pub fn comment<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<Input<'s>>>
where
    for<'a> Input<'a>: Stream<Token = char>,
{
    trace(
        "comment",
        delimited(
            "//",
            repeat_till::<_, _, (), _, _, _, _>(0.., any, alt((tracking_new_line, eof.void()))),
            opt(tracking_new_line),
        ),
    )
    .void()
    .parse_next(input)
}

pub fn tracking_new_line<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<Input<'s>>>
where
    for<'a> Input<'a>: Stream<Token = char>,
{
    '\n'.parse_next(input)?;
    input.state.inc_line();
    Ok(())
}

pub fn whitespace<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<Input<'s>>> {
    trace(
        "whitespace",
        repeat::<_, _, (), _, _>(
            0..,
            alt((one_of([' ', '\t', '\r']).void(), comment, tracking_new_line)),
        ),
    )
    .parse_next(input)
}

pub fn parse_error<'s, Output>(
    ty: ParseErrorType,
) -> impl Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>> {
    trace("parse_error", move |input: &mut Input<'s>| {
        let token =
            preceded(whitespace, take_till(0.., ['\n', '\r', ' ', '\t'])).parse_next(input)?;
        Err(ErrMode::Cut(Error::Parse(ParseError::new(
            ty.clone(),
            token,
            input,
        ))))
    })
}

pub fn space_wrap<'s, Output, P>(
    inner: P,
) -> impl Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>
where
    P: Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>,
{
    trace("space_wrap", delimited(whitespace, inner, whitespace))
}

pub fn or_parse_error<'s, Output, P>(
    inner: P,
    ty: ParseErrorType,
) -> impl Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>
where
    P: Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>,
{
    trace("or_parse_error", alt((inner, parse_error(ty))))
}

pub fn full_word<'s>(
    word: &'static str,
) -> impl Parser<Input<'s>, <Input<'s> as Stream>::Slice, ErrMode<Error<Input<'s>>>> {
    trace(
        word,
        space_wrap(alpha1.verify(move |id: <Input as Stream>::Slice| id == word)),
    )
}

#[derive(Default, Clone)]
pub enum Value {
    #[default]
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<str>),
    NativeCallable(NativeCallable),
    Callable(Callable),
    Class(Rc<Class>),
    Instance(Instance),
    Method(Method),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Self::String(s) => f.debug_tuple("String").field(s).finish(),
            Self::NativeCallable(NativeCallable { arg_num, func: _ }) => {
                write!(f, "<native fn({arg_num} args)>")
            }
            Self::Callable(Callable {
                name,
                param_names: args,
                ..
            }) => {
                write!(f, "<fn {name} ({} args)>", args.len())
            }
            Self::Class(class) => {
                write!(
                    f,
                    "<class {name} {methods:#?}>",
                    name = class.name,
                    methods = class.methods
                )
            }
            Self::Instance(Instance { class, .. }) => {
                write!(f, "<class instance {}>", class.name)
            }
            Self::Method(Method {
                name, param_names, ..
            }) => {
                write!(f, "<fn {name} ({} args)>", param_names.len())
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::String(l), Self::String(r)) => l == r,
            (
                Self::Callable(Callable {
                    name: l_name,
                    param_names: l_args,
                    body: l_stmt,
                    ..
                }),
                Self::Callable(Callable {
                    name: r_name,
                    param_names: r_args,
                    body: r_stmt,
                    ..
                }),
            ) => l_args == r_args && l_stmt == r_stmt && l_name == r_name,
            (
                Self::NativeCallable(NativeCallable {
                    arg_num: _,
                    func: _,
                }),
                Self::NativeCallable(NativeCallable {
                    arg_num: _,
                    func: _,
                }),
            ) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::NativeCallable(NativeCallable {
                arg_num: _,
                func: _,
            }) => write!(f, "<native fn>"),
            Self::Callable(Callable { name, .. }) | Self::Method(Method { name, .. }) => {
                write!(f, "<fn {name}>")
            }
            Self::Class(class) => {
                write!(f, "{name}", name = class.name)
            }
            Self::Instance(Instance { class, .. }) => {
                write!(f, "{class} instance", class = class.name)
            }
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Self::Number(n)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Self::String(s.into())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::String(s.into())
    }
}

impl From<usize> for Value {
    fn from(n: usize) -> Self {
        Self::Number(n as f64)
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        matches!(
            value,
            Value::Bool(true) | Value::Number(_) | Value::String(_)
        )
    }
}
impl From<&Value> for bool {
    fn from(value: &Value) -> Self {
        matches!(
            value,
            &Value::Bool(true) | &Value::Number(_) | &Value::String(_)
        )
    }
}

pub type NativeFn = Arc<dyn Fn(&[Value], &Context) -> Value>;

#[derive(Clone)]
pub struct NativeCallable {
    pub arg_num: usize,
    pub func: NativeFn,
}

#[derive(Debug, Default, Clone)]
pub struct Class {
    pub name: Rc<str>,
    pub methods: Rc<[Value]>,
    pub context: Context,
}

impl Class {
    pub fn eval_class_call(
        &self,
        param_values: &[Value],
        caller_env: &Context,
    ) -> Result<Value, EvaluateError> {
        let Class {
            name,
            methods,
            ..
            // context: class_context,
        } = self;
        let instance_env = caller_env.child();
        log::debug!("Creating class instance {name}, Context: {instance_env:?}");
        let fields = Rc::new(RefCell::new(
            methods
                .iter()
                .map(|func| {
                    log::trace!("Class method: {func:?}");
                    let (name, value) = match func {
                        Value::Callable(callable) => (
                            callable.name.clone(),
                            Value::Callable(Callable {
                                env: instance_env.clone(),
                                ..callable.clone()
                            }),
                        ),
                        _ => return Err(EvaluateError::ClassMethodNotFunction(name.to_string())),
                    };
                    Ok((name, value))
                })
                .collect::<Result<HashMap<Rc<str>, Value>, _>>()?,
        ));
        log::trace!("Class instance fields: {fields:#?}");
        let instance = Instance {
            class: Rc::new(self.clone()),
            fields,
            context: instance_env.clone(),
        };
        let this = Value::Instance(instance.clone());
        instance_env.declare("this", this.clone(), Some(0))?;
        log::debug!(
            "Class instance Context after defined: {instance_env:?}, Instance: {instance:?}"
        );
        let borrow = instance.fields.borrow();
        let get = borrow.get("init").cloned();
        // Ensure we're done borrowing `fields` before calling init
        drop(borrow);
        if let Some(init) = get {
            let Value::Callable(ref callable) = init else {
                return Err(EvaluateError::ClassMethodNotFunction("init".to_string()));
            };
            log::debug!("Calling class contsructor: [{init}]({methods:?}): {callable:?}");
            let value = FnCall::eval_callable(callable, param_values, &instance_env)?;
            assert_eq!(value, this);
        } else if !param_values.is_empty() {
            return Err(EvaluateError::ArgumentMismatch {
                expected: 0,
                got: 2,
            });
        }
        Ok(this)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: Rc<RefCell<HashMap<Rc<str>, Value>>>,
    pub context: Context,
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.class, &other.class) && self.fields == other.fields
    }
}

#[derive(Debug, Clone)]
pub struct Callable {
    pub name: Rc<str>,
    pub param_names: Rc<[Rc<str>]>,
    pub body: Rc<Statement>,
    pub env: Context,
    pub initializer: bool,
}

impl Default for Callable {
    fn default() -> Self {
        Self {
            name: Rc::default(),
            param_names: Rc::default(),
            body: Rc::new(Statement::Block(Rc::default())),
            env: Context::default(),
            initializer: false,
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.param_names == other.param_names
            && self.body == other.body
            && self.initializer == other.initializer
            && self.env == other.env
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub instance: Option<Rc<Instance>>,
    pub name: Rc<str>,
    pub param_names: Rc<[Rc<str>]>,
    pub body: Rc<Statement>,
    pub env: Context,
    pub initializer: bool,
}

impl Default for Method {
    fn default() -> Self {
        Self {
            instance: None,
            name: Rc::default(),
            param_names: Rc::default(),
            body: Rc::new(Statement::Block(Rc::default())),
            env: Context::default(),
            initializer: false,
        }
    }
}

impl PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        self.instance == other.instance
            && self.name == other.name
            && self.param_names == other.param_names
            && self.body == other.body
            && self.initializer == other.initializer
    }
}
