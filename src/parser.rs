use std::{rc::Rc, sync::Arc};

use ast::Statement;
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
    trace(format!("tracking_new_line: {}", input.state.line()), '\n').parse_next(input)?;
    input.state.inc_line();
    Ok(())
}

pub fn whitespace<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<Input<'s>>> {
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
    delimited(whitespace, inner, whitespace)
}

pub fn or_parse_error<'s, Output, P>(
    inner: P,
    ty: ParseErrorType,
) -> impl Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>
where
    P: Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>,
{
    alt((inner, parse_error(ty)))
}

pub fn full_word<'s>(
    word: &'static str,
) -> impl Parser<Input<'s>, <Input<'s> as Stream>::Slice, ErrMode<Error<Input<'s>>>> {
    space_wrap(alpha1.verify(move |id: <Input as Stream>::Slice| id == word))
}

#[derive(Default, Clone)]
pub enum Value {
    #[default]
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<str>),
    NativeCallable(usize, Arc<dyn Fn(Vec<Value>) -> Value>),
    Callable(Rc<str>, Rc<[Rc<str>]>, Rc<Statement>, Context),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Self::String(s) => f.debug_tuple("String").field(s).finish(),
            Self::NativeCallable(i, _fn) => write!(f, "<native fn({i} args)>"),
            Self::Callable(name, args, _stmt, _env) => write!(f, "<fn {name} ({} args)>", args.len()),
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
                Self::Callable(l_name, l_args, l_stmt, _l_env),
                Self::Callable(r_name, r_args, r_stmt, _r_env),
            ) => l_args == r_args && l_stmt == r_stmt && l_name == r_name,
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
            Self::NativeCallable(_, _) => write!(f, "<native fn>"),
            Self::Callable(name, _args, _stmt, _env) => {
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
