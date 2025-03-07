use std::fmt::Display;

use winnow::{
    error::{AddContext, ErrMode, FromExternalError, ParserError},
    stream::Stream,
};

use crate::parser::{Input, Value};

#[derive(Debug)]
pub enum Error<'s, S: Stream> {
    Winnow(S),

    Tokenize(String),

    External {
        cause: Box<dyn std::error::Error + Send + Sync + 'static>,
        input: S,
    },

    Parse(ParseError<'s>),

    Evaluate(EvaluateError),
}

impl<S> Display for Error<'_, S>
where
    S: Stream + Display,
    S::Slice: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Winnow(input) => write!(f, "Winnow error at '{input}'"),
            Self::Tokenize(message) => write!(f, "Tokenize error: {message}"),
            Self::External { cause, input } => {
                write!(f, "External error at '{input}': {cause}")
            }
            Self::Parse(err) => write!(f, "{err}"),
            Self::Evaluate(err) => write!(f, "{err}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorType {
    Expected(&'static str),
    UndefinedVariable(String),
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorType::Expected(s) => write!(f, "Expect {s}."),
            ParseErrorType::UndefinedVariable(name) => write!(f, "Undefined variable: `{name}`"),
        }
    }
}

#[derive(Debug)]
pub struct ParseError<'s> {
    pub ty: ParseErrorType,
    pub token: <Input<'s> as Stream>::Slice,
    pub input: Input<'s>,
    pub line: usize,
}

impl<'s> ParseError<'s> {
    pub fn new(ty: ParseErrorType, token: <Input<'s> as Stream>::Slice, input: Input<'s>) -> Self {
        let line = input.state.line();
        Self {
            ty,
            token,
            input,
            line,
        }
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] Error at '{}': {}",
            self.line, self.token, self.ty
        )
    }
}

impl<'s, S: Stream> From<ParseError<'s>> for Error<'s, S> {
    fn from(err: ParseError<'s>) -> Self {
        Self::Parse(err)
    }
}

impl<'s, S: Stream> From<winnow::error::ParseError<S, Error<'s, S>>> for Error<'s, S> {
    fn from(value: winnow::error::ParseError<S, Error<'s, S>>) -> Self {
        value.into_inner()
    }
}

impl<S> std::error::Error for Error<'_, S>
where
    S: Stream + Display,
    S::Slice: Display,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::External { cause, .. } => Some(cause.as_ref()),
            _ => None,
        }
    }
}

impl<S: Stream + Clone> ParserError<S> for Error<'_, S> {
    type Inner = Self;

    fn from_input(input: &S) -> Self {
        Self::Winnow(input.clone())
    }

    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }
}

impl<C, S: Stream> AddContext<S, C> for Error<'_, S> {
    #[inline]
    fn add_context(
        self,
        _input: &S,
        _token_start: &<S as Stream>::Checkpoint,
        _context: C,
    ) -> Self {
        self
    }
}

impl<S: Stream + Clone, E: std::error::Error + Send + Sync + 'static> FromExternalError<S, E>
    for Error<'_, S>
{
    #[inline]
    fn from_external_error(input: &S, e: E) -> Self {
        Error::External {
            cause: Box::new(e),
            input: input.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EvaluateError {
    TypeMismatch { expected: String },
    UndefinedVariable(String),
    AlreadyDefined(String),
    CannotAssignToSelf(String),
    ReservedWord(String),
    ArgumentMismatch { expected: usize, got: usize },
    FunctionBodyNotBlock(String),
    NotCallable(Value),
    Return(Value),
}

impl From<EvaluateError> for Error<'_, Input<'_>> {
    fn from(err: EvaluateError) -> Self {
        Error::Evaluate(err)
    }
}

impl Display for EvaluateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch { expected } => write!(f, "Operands must be {expected}"),
            Self::UndefinedVariable(name) => write!(f, "Undefined variable: '{name}'"),
            Self::AlreadyDefined(name) => write!(
                f,
                "Error at '{name}': Already a variable with this name in this scope."
            ),
            Self::ReservedWord(name) => write!(f, "Cannot assign to reserved word: '{name}'"),
            Self::ArgumentMismatch { expected, got } => {
                write!(f, "Agrument mismatch: expected {expected}, found {got}.")
            }
            Self::FunctionBodyNotBlock(name) => {
                write!(f, "Function body must be a block: '{name}'.")
            }
            Self::NotCallable(v) => write!(f, "Expected callable, found {v:?}."),
            Self::Return(v) => write!(f, "Returned value: {v}."),
            Self::CannotAssignToSelf(name) => write!(f, "Error at '{name}': Can't read local variable in its own initializer."),
        }
    }
}
