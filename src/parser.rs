use std::sync::Arc;

use ast::Statement;
use winnow::{
    combinator::{alt, delimited, eof, opt, repeat, repeat_till, trace},
    error::ErrMode,
    stream::Stream,
    token::{any, one_of},
    ModalResult, Parser,
};

use crate::{
    error::{Error, EvaluateError, ParseError},
    interpreter::EnvironmentView,
    parser::state::Stateful,
};

pub mod ast;
pub mod expr;
pub mod state;

pub type InputStream<'s> = &'s str;
pub type Input<'s> = Stateful<InputStream<'s>>;

pub trait Evaluate {
    fn evaluate<'s, 'env>(&'s self, _: &'env mut EnvironmentView) -> Result<Value, EvaluateError>
    where
        's: 'env;
}

pub trait Run {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>>;
}

pub fn comment<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<'s, Input<'s>>>
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

pub fn tracking_new_line<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<'s, Input<'s>>>
where
    for<'a> Input<'a>: Stream<Token = char>,
{
    trace(format!("tracking_new_line: {}", input.state.line()), '\n').parse_next(input)?;
    input.state.inc_line();
    Ok(())
}

pub fn whitespace<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<'s, Input<'s>>> {
    trace(
        "tracking_multispace",
        repeat::<_, _, (), _, _>(
            0..,
            alt((one_of([' ', '\t', '\r']).void(), comment, tracking_new_line)),
        ),
    )
    .parse_next(input)
}

pub fn parse_error<'s, Output>(
    msg: &'static str,
) -> impl Parser<Input<'s>, Output, ErrMode<Error<'s, Input<'s>>>> {
    trace("parse_error", move |input: &mut Input<'s>| {
        whitespace.parse_next(input)?;
        Err(ErrMode::Cut(Error::Parse(ParseError::new(
            msg.to_string(),
            *input,
        ))))
    })
}

#[derive(Default, Clone)]
pub enum Value {
    #[default]
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    NativeCallable(usize, Arc<dyn Fn(Vec<Value>) -> Value>),
    Callable(String, Vec<String>, Arc<Statement>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Self::String(s) => f.debug_tuple("String").field(s).finish(),
            Self::NativeCallable(i, _) => write!(f, "<native fn({i} args)>"),
            Self::Callable(name, args, stmt) => f
                .debug_struct("Callable")
                .field("name", name)
                .field("args", args)
                .field("body", stmt)
                .finish(),
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
            (Self::Callable(l_name, l_args, l_stmt), Self::Callable(r_name, r_args, r_stmt)) => {
                l_args == r_args && l_stmt == r_stmt && l_name == r_name
            }
            (Self::NativeCallable(_, _), Self::NativeCallable(_, _)) => false,
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
            Self::NativeCallable(n, _) => write!(f, "<native fn({n} args)>"),
            Self::Callable(name, _, _) => {
                write!(f, "<fn {name}>")
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
        Self::String(s.to_string())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::String(s)
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
