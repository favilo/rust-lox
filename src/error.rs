use std::fmt::Display;

use winnow::{
    error::{AddContext, FromExternalError, ParserError},
    stream::Stream,
};

use crate::parser::{Input, Value};

#[derive(Debug)]
pub enum Error<S: Stream> {
    Winnow(S),

    Tokenize(String),

    External {
        cause: Box<dyn std::error::Error + Send + Sync + 'static>,
        input: S,
    },

    Parse(ParseError),

    Evaluate(EvaluateError),
}

impl<S> Display for Error<S>
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
    InvalidOperator,
    UnterminatedString,
    TooManyArguments,
    TooManyParameters,
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorType::Expected(s) => write!(f, "Expect {s}."),
            ParseErrorType::UndefinedVariable(name) => write!(f, "Undefined variable: `{name}`"),
            ParseErrorType::InvalidOperator => write!(f, "Invalid operator"),
            ParseErrorType::UnterminatedString => write!(f, "Unterminated string."),
            ParseErrorType::TooManyArguments => write!(f, "Can't have more than 255 arguments."),
            ParseErrorType::TooManyParameters => write!(f, "Can't have more than 255 parameters."),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub ty: ParseErrorType,
    pub token: String,
    pub line: usize,
}

impl ParseError {
    pub fn new(ty: ParseErrorType, token: <Input as Stream>::Slice, input: &Input) -> Self {
        let line = input.state.line();
        let token = token.to_string();
        Self { ty, token, line }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] Error at '{}': {}",
            self.line, self.token, self.ty
        )
    }
}

impl<S: Stream> From<ParseError> for Error<S> {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl<S: Stream> From<winnow::error::ParseError<S, Error<S>>> for Error<S> {
    fn from(value: winnow::error::ParseError<S, Error<S>>) -> Self {
        value.into_inner()
    }
}

impl<S> std::error::Error for Error<S>
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

impl<S: Stream + Clone> ParserError<S> for Error<S> {
    type Inner = Self;

    fn from_input(input: &S) -> Self {
        Self::Winnow(input.clone())
    }

    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }
}

impl<C, S: Stream> AddContext<S, C> for Error<S> {
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
    for Error<S>
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
    TopLevelReturn,
    Return(Value),
    StackOverflow,
    TooLargeBody(usize),
}

impl From<EvaluateError> for Error<Input<'_>> {
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
            Self::CannotAssignToSelf(name) => write!(
                f,
                "Error at '{name}': Can't read local variable in its own initializer."
            ),
            Self::TopLevelReturn => write!(f, "Can't return from top-level code."),
            Self::StackOverflow => write!(f, "Stack overflow."),
            Self::TooLargeBody(size) => write!(f, "Block body too large: {size}"),
        }
    }
}
