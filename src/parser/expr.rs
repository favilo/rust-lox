use std::{hash::Hash, iter::once, sync::Arc};
use winnow::{
    ascii::{digit1, Caseless},
    combinator::{alt, cut_err, delimited, empty, fail, opt, preceded, repeat, terminated, trace},
    dispatch,
    error::{ErrMode, ParserError},
    seq,
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    token::{any, one_of, take_till, take_while},
    ModalResult, Parser,
};

use crate::{
    error::{Error, EvaluateError},
    interpreter::EnvironmentView,
    parser::state::{State, Stateful},
};

use super::{parse_error, whitespace, Evaluate, Input, Value};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Group(Box<Expr>),
    Unary(Unary),
    Binary(Binary),
    Assignment(String, Box<Expr>),
    FnCall(Box<Expr>, Vec<Expr>),
}

impl Evaluate for Expr {
    fn evaluate<'s, 'env>(&'s self, env: &'env mut EnvironmentView) -> Result<Value, EvaluateError>
    where
        's: 'env,
    {
        match self {
            Self::Literal(l) => l.evaluate(env),
            Self::Group(e) => e.evaluate(env),
            Self::Unary(u) => u.evaluate(env),
            Self::Binary(b) => b.evaluate(env),
            Self::Assignment(id, e) => {
                let value = e.evaluate(env)?;
                env.set(id, value.clone());
                Ok(value)
            }
            Self::FnCall(expr, args) => {
                let args = args
                    .iter()
                    .map(|e| e.evaluate(env))
                    .collect::<Result<Vec<_>, _>>()?;
                let callable = expr.evaluate(env)?;
                match callable {
                    Value::NativeCallable(n, f) => {
                        // TODO: Add currying
                        if n != args.len() {
                            return Err(EvaluateError::ArgumentMismatch {
                                expected: n,
                                got: args.len(),
                            });
                        }
                        Ok(f(args))
                    }
                    Value::Callable(_name, names, stmt) => {
                        // TODO: Add currying
                        if names.len() != args.len() {
                            return Err(EvaluateError::ArgumentMismatch {
                                expected: names.len(),
                                got: args.len(),
                            });
                        }
                        let mut fn_env = env.child_view();
                        for (name, value) in names.iter().zip(args.iter()) {
                            fn_env.define(name, value.clone());
                        }
                        let stmt = Arc::unwrap_or_clone(stmt);
                        let value = stmt.evaluate(&mut fn_env)?.clone();
                        Ok(value)
                    }
                    _ => Err(EvaluateError::NotCallable(callable)),
                }
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Literal(Literal::Number(n)) => write!(f, "{:?}", n),
            Self::Literal(l) => write!(f, "{}", l),
            Self::Unary(u) => write!(f, "{}", u),
            Self::Binary(b) => write!(f, "{}", b),
            Self::Group(e) => write!(f, "(group {})", e),
            Self::Assignment(id, e) => write!(f, "(= {} {})", id, e),
            Self::FnCall(expr, args) => {
                write!(
                    f,
                    "({} {})",
                    expr,
                    args.iter().map(|e| e.to_string()).collect::<String>()
                )
            }
        }
    }
}

impl Expr {
    pub fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        trace(
            "expr",
            delimited(
                whitespace,
                alt((Expr::assignment, Expr::logical_or)),
                whitespace,
            ),
        )
        .parse_next(input)
    }

    pub fn assignment<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        seq!(Literal::identifier, _: whitespace, _: "=", _: whitespace, Expr::parser)
            .map(|(id, e)| Expr::Assignment(id, Box::new(e)))
            .parse_next(input)
    }

    pub fn logical_or<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = trace("First comparison", Expr::logical_and).parse_next(input)?;

        trace(
            "rest of logical_or",
            repeat(
                0..,
                (
                    "or",
                    alt((Expr::logical_and, parse_error("Expect expression."))),
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (_, val): (<Input as Stream>::Slice, Expr)| {
                    Expr::Binary(Binary::Or(Box::new(acc), Box::new(val)))
                },
            ),
        )
        .parse_next(input)
    }

    pub fn logical_and<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = trace("First comparison", Expr::equality).parse_next(input)?;

        trace(
            "rest of logical_and",
            repeat(
                0..,
                (
                    "and",
                    alt((Expr::equality, parse_error("Expect expression."))),
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (_, val): (<Input as Stream>::Slice, Expr)| {
                    Expr::Binary(Binary::And(Box::new(acc), Box::new(val)))
                },
            ),
        )
        .parse_next(input)
    }

    pub fn equality<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = trace("First comparison", Expr::comparison).parse_next(input)?;

        trace(
            "rest of equality",
            repeat(
                0..,
                (
                    alt(("==", "!=")),
                    alt((Expr::comparison, parse_error("Expect expression."))),
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (op, val): (<Input as Stream>::Slice, Expr)| match op.as_bstr() {
                    b"==" => Expr::Binary(Binary::Equals(Box::new(acc), Box::new(val))),
                    b"!=" => Expr::Binary(Binary::NotEquals(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn comparison<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = trace("First term", Expr::term).parse_next(input)?;

        trace(
            "rest of comparison",
            repeat(
                0..,
                (
                    alt(("<=", ">=", "<", ">")),
                    alt((Expr::term, parse_error("Expect expression."))),
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (op, val): (<Input as Stream>::Slice, Expr)| match op.as_bstr() {
                    b"<=" => Expr::Binary(Binary::LessEq(Box::new(acc), Box::new(val))),
                    b">=" => Expr::Binary(Binary::GreaterEq(Box::new(acc), Box::new(val))),
                    b"<" => Expr::Binary(Binary::LessThan(Box::new(acc), Box::new(val))),
                    b">" => Expr::Binary(Binary::GreaterThan(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn term<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = trace("First factor", Expr::factor).parse_next(input)?;

        trace(
            "rest of term",
            repeat(
                0..,
                (
                    one_of(['+', '-']),
                    alt((Expr::factor, parse_error("Expect expression."))),
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (op, val): (<Input as Stream>::Token, Expr)| match op.as_char() {
                    '+' => Expr::Binary(Binary::Add(Box::new(acc), Box::new(val))),
                    '-' => Expr::Binary(Binary::Sub(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn factor<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = trace("First unary", Expr::unary).parse_next(input)?;

        trace(
            "rest of factor",
            repeat(
                0..,
                (
                    one_of(['*', '/']),
                    alt((Expr::unary, parse_error("Expect expression."))),
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (op, val): (<Input as Stream>::Token, Expr)| match op.as_char() {
                    '*' => Expr::Binary(Binary::Mul(Box::new(acc), Box::new(val))),
                    '/' => Expr::Binary(Binary::Div(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn unary<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        #[derive(Debug, Clone, Copy)]
        enum Sign {
            Negate,
            Not,
        }

        let operator = trace(
            "operator",
            dispatch! {any.map(AsChar::as_char);
                '-' => empty.value(Sign::Negate),
                '!' => empty.value(Sign::Not),
                _ => fail,
            },
        );

        alt((
            trace(
                "unary",
                (
                    delimited(whitespace, operator, whitespace),
                    terminated(Expr::unary, whitespace),
                )
                    .map(|(neg, e)| match neg {
                        Sign::Negate => Expr::Unary(Unary::Negate(Box::new(e))),
                        Sign::Not => Expr::Unary(Unary::Not(Box::new(e))),
                    }),
            ),
            trace("no unary", Expr::call),
        ))
        .parse_next(input)
    }

    pub fn call<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<'s, Input<'s>>> {
        let init = Expr::primary(input)?;
        trace(
            "function calls",
            repeat(0.., delimited("(", Expr::arguments, (")", whitespace))).fold(
                move || init.clone(),
                |acc, args: Vec<Expr>| Expr::FnCall(Box::new(acc), args),
            ),
        )
        .parse_next(input)
    }

    fn arguments<'s>(input: &mut Input<'s>) -> ModalResult<Vec<Expr>, Error<'s, Input<'s>>> {
        let head = trace("first argument", opt(Expr::parser)).parse_next(input)?;
        let Some(head) = head else {
            return Ok(vec![]);
        };

        let rest: Vec<Expr> = trace(
            "rest of arguments",
            repeat(0.., preceded((whitespace, ","), Expr::parser)),
        )
        .parse_next(input)?;
        Ok(once(head).chain(rest).collect::<Vec<Expr>>())
    }

    pub fn primary<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        trace(
            "primary",
            delimited(
                whitespace,
                alt((Expr::parenthesis, Literal::parser.map(Expr::Literal))),
                whitespace,
            ),
        )
        .parse_next(input)
    }

    fn parenthesis<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        trace(
            "parenthesis",
            preceded(
                "(",
                delimited(
                    whitespace,
                    terminated(Self::parser, whitespace),
                    alt((")", parse_error("Expect expression."))),
                ),
            ),
        )
        .map(|e| Self::Group(Box::new(e)))
        .parse_next(input)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unary {
    Negate(Box<Expr>),
    Not(Box<Expr>),
}

impl Evaluate for Unary {
    fn evaluate<'s, 'env>(&'s self, env: &'env mut EnvironmentView) -> Result<Value, EvaluateError>
    where
        's: 'env,
    {
        match self {
            Self::Negate(e) => Ok(match e.evaluate(env)? {
                Value::Number(n) => Value::Number(-n),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "number".into(),
                    })
                }
            }),
            Self::Not(e) => Ok(match e.evaluate(env)? {
                Value::Nil => Value::Bool(true),
                Value::Bool(b) => Value::Bool(!b),
                _ => Value::Bool(false),
            }),
        }
    }
}

impl std::fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Negate(e) => write!(f, "(- {})", e),
            Self::Not(e) => write!(f, "(! {})", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binary {
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    NotEquals(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
}

impl Evaluate for Binary {
    fn evaluate<'s, 'env>(&'s self, env: &'env mut EnvironmentView) -> Result<Value, EvaluateError>
    where
        's: 'env,
    {
        match self {
            Self::Mul(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l * r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::Div(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l / r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::Add(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l + r),
                (Value::String(s), Value::String(t)) => Value::from(format!("{}{}", s, t)),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "two numbers or two strings".into(),
                    })
                }
            }),
            Self::Sub(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l - r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::LessThan(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l < r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::GreaterThan(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l > r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::LessEq(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l <= r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::GreaterEq(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l >= r),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "numbers".into(),
                    })
                }
            }),
            Self::Equals(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l == r),
                (Value::String(s), Value::String(t)) => Value::from(s == t),
                (Value::Nil, Value::Nil) => Value::Bool(true),
                (Value::Bool(true), Value::Bool(true)) => Value::Bool(true),
                (Value::Bool(false), Value::Bool(false)) => Value::Bool(true),
                _ => Value::Bool(false),
            }),
            Self::NotEquals(l, r) => Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
                (Value::Number(l), Value::Number(r)) => Value::from(l != r),
                (Value::String(s), Value::String(t)) => Value::from(s != t),
                (Value::Nil, Value::Nil) => Value::Bool(false),
                (Value::Bool(true), Value::Bool(true)) => Value::Bool(false),
                (Value::Bool(false), Value::Bool(false)) => Value::Bool(false),
                _ => Value::Bool(true),
            }),
            Self::Or(l, r) => {
                let a = l.evaluate(env)?;
                log::debug!("Or: {a}");
                Ok(if bool::from(&a) {
                    a
                } else {
                    let b = r.evaluate(env)?;
                    log::debug!("Or false: {b}");
                    b
                })
            }
            Self::And(l, r) => {
                let a = l.evaluate(env)?;
                log::debug!("And: {a}");
                Ok(if bool::from(&a) {
                    let b = r.evaluate(env)?;
                    log::debug!("And true: {b}");
                    b
                } else {
                    a
                })
            }
        }
    }
}

impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mul(l, r) => write!(f, "(* {} {})", l, r),
            Self::Div(l, r) => write!(f, "(/ {} {})", l, r),
            Self::Add(l, r) => write!(f, "(+ {} {})", l, r),
            Self::Sub(l, r) => write!(f, "(- {} {})", l, r),
            Self::LessThan(l, r) => write!(f, "(< {} {})", l, r),
            Self::GreaterThan(l, r) => write!(f, "(> {} {})", l, r),
            Self::LessEq(l, r) => write!(f, "(<= {} {})", l, r),
            Self::GreaterEq(l, r) => write!(f, "(>= {} {})", l, r),
            Self::Equals(l, r) => write!(f, "(== {} {})", l, r),
            Self::NotEquals(l, r) => write!(f, "(!= {} {})", l, r),
            Self::Or(l, r) => write!(f, "(or {} {})", l, r),
            Self::And(l, r) => write!(f, "(and {} {})", l, r),
        }
    }
}

#[derive(Clone)]
pub enum Literal {
    True,
    False,
    Nil,
    Number(f64),
    String(String),
    Id(String),
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::Nil => write!(f, "Nil"),
            Self::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Self::String(s) => f.debug_tuple("String").field(s).finish(),
            Self::Id(id) => f.debug_tuple("Id").field(id).finish(),
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Id(l0), Self::Id(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        if b {
            Self::True
        } else {
            Self::False
        }
    }
}

impl From<f64> for Literal {
    fn from(n: f64) -> Self {
        Self::Number(n)
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        Self::String(s.to_string())
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<usize> for Literal {
    fn from(n: usize) -> Self {
        Self::Number(n as f64)
    }
}

impl From<Literal> for bool {
    fn from(value: Literal) -> Self {
        matches!(
            value,
            Literal::True | Literal::Number(_) | Literal::String(_)
        )
    }
}

impl From<&Literal> for bool {
    fn from(value: &Literal) -> Self {
        matches!(
            value,
            Literal::True | Literal::Number(_) | Literal::String(_)
        )
    }
}

impl Evaluate for Literal {
    fn evaluate<'s, 'env>(&'s self, env: &'env mut EnvironmentView) -> Result<Value, EvaluateError>
    where
        's: 'env,
    {
        Ok(match self {
            Self::Id(s) => env
                .get(s)
                .cloned()
                .ok_or_else(|| EvaluateError::UndefinedVariable(s.clone()))?,
            // TODO: might change how we handle literals that are functions.
            Self::Nil => Value::Nil,
            Self::True => Value::Bool(true),
            Self::False => Value::Bool(false),
            Self::Number(n) => Value::Number(*n),
            Self::String(s) => Value::String(s.clone()),
        })
    }
}

impl Literal {
    pub(crate) fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        trace(
            "literal",
            delimited(
                whitespace,
                alt((
                    "true".value(Self::True),
                    "false".value(Self::False),
                    "nil".value(Self::Nil),
                    Self::number,
                    Self::string,
                    Self::identifier.map(Self::Id),
                )),
                whitespace,
            ),
        )
        .parse_next(input)
    }

    fn number<S, E>(input: &mut S) -> ModalResult<Self, E>
    where
        for<'a> S: Stream
            + StreamIsPartial
            + Compare<&'a str>
            + Compare<Caseless<&'a str>>
            + AsBStr
            + Compare<char>,
        S::Slice: Eq + Hash + AsBStr + ParseSlice<f64> + Clone,
        S::Token: AsChar + Clone,
        S::IterOffsets: Clone,
        E: ParserError<S>,
    {
        trace("number", move |input: &mut S| {
            let it = alt(((".", digit1).void(), (digit1, opt(('.', digit1))).void()))
                .take()
                .parse_next(input)?;
            it.parse_slice()
                .map(Self::Number)
                .ok_or_else(|| ParserError::from_input(input))
        })
        .parse_next(input)
    }

    fn string<S, E>(input: &mut S) -> ModalResult<Self, E>
    where
        for<'a> S: Stream
            + StreamIsPartial
            + Compare<&'a str>
            + Compare<Caseless<&'a str>>
            + AsBStr
            + Compare<char>,
        S::Slice: Eq + Hash + AsBStr + ParseSlice<f64> + Clone,
        S::Token: AsChar + Clone,
        S::IterOffsets: Clone,
        E: ParserError<S>,
    {
        trace(
            "string",
            preceded("\"", cut_err(terminated(take_till(0.., '"'), "\""))),
        )
        .map(|s: S::Slice| Self::String(std::str::from_utf8(s.as_bstr()).unwrap().to_string()))
        .parse_next(input)
    }

    pub(crate) fn identifier<'s>(
        input: &mut Input<'s>,
    ) -> ModalResult<String, Error<'s, Input<'s>>> {
        let id = trace(
            "identifier",
            (
                any.verify(|c: &<Input as Stream>::Token| {
                    let c = c.as_char();
                    c.is_alphabetic() || c == '_'
                }),
                take_while(0.., |c: <Input as Stream>::Token| {
                    let c = c.as_char();
                    c.is_alphanumeric() || c == '_'
                }),
            ),
        )
        .take()
        .map(ToString::to_string)
        .parse_next(input)?;
        if matches!(
            id.as_ref(),
            "and"
                | "class"
                | "else"
                | "false"
                | "for"
                | "fun"
                | "if"
                | "nil"
                | "or"
                | "print"
                | "return"
                | "super"
                | "this"
                | "true"
                | "var"
                | "while"
        ) {
            Err(ErrMode::Backtrack(Error::from(
                EvaluateError::ReservedWord(id),
            )))
        } else {
            Ok(id)
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
            Self::Number(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Id(s) => write!(f, "{s}"),
        }
    }
}

pub fn parse(input: &str) -> Result<Expr, Error<'_, Input<'_>>> {
    Ok(Expr::parser.parse(Stateful::new(input, State::new(1)))?)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_bool() -> anyhow::Result<()> {
        let input = "true\n";
        let res = parse(input).unwrap();
        assert_eq!(res, Expr::Literal(Literal::True));

        let input = "false   \t";
        let res = parse(input).unwrap();
        assert_eq!(res, Expr::Literal(Literal::False));
        Ok(())
    }

    #[test]
    fn test_nil() -> anyhow::Result<()> {
        let input = "     nil   ";
        let res = parse(input).unwrap();
        assert_eq!(res, Expr::Literal(Literal::Nil));
        Ok(())
    }

    #[test]
    fn test_number() -> anyhow::Result<()> {
        let input = "123.45";
        let res = parse(input).unwrap();
        assert_eq!(res, Expr::Literal(Literal::Number(123.45)));
        Ok(())
    }

    #[test]
    fn test_string() -> anyhow::Result<()> {
        let input = "\n \"hello, world!\"";
        let res = parse(input).unwrap();
        assert_eq!(
            res,
            Expr::Literal(Literal::String("hello, world!".to_string()))
        );
        Ok(())
    }

    #[test]
    fn test_unary() -> anyhow::Result<()> {
        let input = "-123.45    ";
        let res = parse(input).unwrap();
        assert_eq!(
            res,
            Expr::Unary(Unary::Negate(Box::new(Expr::Literal(Literal::Number(
                123.45
            )))))
        );

        Ok(())
    }

    #[test]
    fn test_paren() -> anyhow::Result<()> {
        let input = "  !(  -123.45  )\t\n";
        let res = parse(input).unwrap();
        assert_eq!(
            res,
            Expr::Unary(Unary::Not(Box::new(Expr::Group(Box::new(Expr::Unary(
                Unary::Negate(Box::new(Expr::Literal(Literal::Number(123.45))))
            ))))))
        );

        Ok(())
    }

    #[test]
    fn test_term() -> anyhow::Result<()> {
        let input = "123.45 * 67.89 / 10.11";
        let res = parse(input).unwrap();
        assert_eq!(
            res,
            Expr::Binary(Binary::Div(
                Box::new(Expr::Binary(Binary::Mul(
                    Box::new(Expr::Literal(Literal::Number(123.45))),
                    Box::new(Expr::Literal(Literal::Number(67.89)))
                ))),
                Box::new(Expr::Literal(Literal::Number(10.11)))
            ))
        );

        Ok(())
    }

    #[test]
    fn test_term_codecrafters() -> anyhow::Result<()> {
        let input = "(76 * -50 / (56 * 42))";
        let res = parse(input).unwrap();
        assert_eq!(
            res.to_string(),
            "(group (/ (* 76.0 (- 50.0)) (group (* 56.0 42.0))))",
        );

        Ok(())
    }

    #[test]
    fn test_add_sub_codecrafters() {
        let input = "(72 * -62 / (42 * 98))";
        let res = parse(input).unwrap();
        assert_eq!(
            res.to_string(),
            "(group (/ (* 72.0 (- 62.0)) (group (* 42.0 98.0))))",
        );
    }

    #[test]
    fn test_hello_plus_world() {
        let input = r#""hello" + "world""#;
        let res = parse(input).unwrap();
        assert_eq!(res.to_string(), "(+ hello world)",);
    }

    #[test]
    fn test_comparison() {
        let input = "83 < 99 < 115";
        let res = parse(input).unwrap();
        assert_eq!(res.to_string(), "(< (< 83.0 99.0) 115.0)");
    }

    #[test]
    fn test_parse_error() -> anyhow::Result<()> {
        let input = "(
            72 +
            )";
        let res = parse(input);
        assert!(res.is_err());
        assert_eq!(
            res.unwrap_err().to_string(),
            "[line 3] Error at ')': Expect expression."
        );
        Ok(())
    }
}
