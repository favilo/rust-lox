use std::{
    hash::Hash,
    iter::{once, zip},
    rc::Rc,
};
use winnow::{
    ModalResult, Parser,
    ascii::{Caseless, digit1},
    combinator::{alt, cut_err, delimited, empty, fail, opt, preceded, repeat, terminated, trace},
    dispatch,
    error::{ErrMode, ParserError},
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    token::{any, one_of, take_till, take_while},
};

use crate::{
    error::{Error, EvaluateError, ParseError, ParseErrorType},
    interpreter::Context,
    parser::{
        ast::Statement,
        state::{State, Stateful},
    },
};

use super::{Evaluate, Input, Value, or_parse_error, space_wrap};

#[cfg(test)]
mod tests;

const F64_PRECISION: f64 = 1e-10;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable(String, Option<usize>),
    Group(Rc<Expr>),
    Unary(Unary),
    Binary(Binary),
    Assignment(Rc<str>, Option<usize>, Rc<Expr>),
    FnCall(Rc<Expr>, Rc<[Expr]>),
}

impl Evaluate for Expr {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        match self {
            Self::Literal(l) => l.evaluate(env),
            Self::Variable(v, depth) => {
                log::trace!("Getting variable [{v}] with depth {depth:?}");
                env.get(v, *depth)
            }
            Self::Group(e) => e.evaluate(env),
            Self::Unary(u) => u.evaluate(env),
            Self::Binary(b) => b.evaluate(env),
            Self::Assignment(id, depth, e) => {
                let value = e.evaluate(env)?;
                log::trace!("Setting variable [{id}] to {value} with depth {depth:?}");
                env.set(id, value.clone(), *depth)?;
                Ok(value)
            }
            Self::FnCall(expr, args) => {
                log::trace!("Function call: {expr}({args:#?})");
                let args = args
                    .iter()
                    .map(|e| e.evaluate(env))
                    .collect::<Result<Vec<_>, _>>()?;
                let callable = expr.evaluate(env)?;
                log::trace!("Callable: {callable}");
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
                    Value::Callable(_name, names, stmt, parent_env) => {
                        // TODO: Add currying
                        if names.len() != args.len() {
                            return Err(EvaluateError::ArgumentMismatch {
                                expected: names.len(),
                                got: args.len(),
                            });
                        }
                        let fn_env = parent_env.child();
                        zip(names.iter(), args.iter()).try_for_each(|(name, arg)| {
                            fn_env.declare(name, arg.clone(), Some(0))
                        })?;
                        log::trace!("Function env: {fn_env:#?}");
                        let stmts = match stmt.as_ref() {
                            Statement::Block(stmts) => stmts.clone(),
                            _ => return Err(EvaluateError::FunctionBodyNotBlock(stmt.to_string())),
                        };
                        let result = stmts
                            .iter()
                            .try_fold(Value::Nil, |_, stmt| stmt.evaluate(&fn_env));
                        if let Err(EvaluateError::Return(n)) = result {
                            Ok(n)
                        } else {
                            Ok(result?)
                        }
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
            Self::Literal(Literal::Number(n)) => write!(f, "{n:?}"),
            Self::Literal(l) => write!(f, "{l}"),
            Self::Variable(v, _scope) => write!(f, "{v}"),
            Self::Unary(u) => write!(f, "{u}"),
            Self::Binary(b) => write!(f, "{b}"),
            Self::Group(e) => write!(f, "(group {e})"),
            Self::Assignment(id, _depth, e) => write!(f, "(= {id} {e})"),
            Self::FnCall(expr, args) => {
                write!(
                    f,
                    "({} {})",
                    expr,
                    args.iter().map(ToString::to_string).collect::<String>()
                )
            }
        }
    }
}

impl Expr {
    pub fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "expr",
            space_wrap(alt((Expr::assignment, Expr::logical_or))),
        )
        .parse_next(input)
    }

    pub fn assignment<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        let name = terminated(Self::identifier, space_wrap("=")).parse_next(input)?;
        let e = Expr::parser.parse_next(input)?;
        let depth = input.state.depth(&name);
        log::debug!("Assigning to variable [{name}] with depth {depth:?}");
        Ok(Expr::Assignment(name.into(), depth, Rc::new(e)))
    }

    fn generic_oper<'s, Ex, Op, Op2>(
        mut inner: Ex,
        mut oper_begin: Op,
        mut oper_end: Op2,
    ) -> impl Parser<Input<'s>, Expr, ErrMode<Error<Input<'s>>>>
    where
        Ex: Parser<Input<'s>, Expr, ErrMode<Error<Input<'s>>>>,
        // + for<'a> Fn(&'a mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>>,
        // Ex: for<'a> Fn(&'a mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>>,
        // Op: Fn(&mut Input<'s>) -> ModalResult<<Input<'s> as Stream>::Slice, Error<Input<'s>>>,
        Op: Parser<Input<'s>, <Input<'s> as Stream>::Slice, ErrMode<Error<Input<'s>>>>,
        Op2: Parser<Input<'s>, <Input<'s> as Stream>::Slice, ErrMode<Error<Input<'s>>>>,
    {
        move |input: &mut Input<'s>| {
            let init = inner.parse_next(input)?;

            let v = repeat(
                0..,
                (
                    trace("oper_begin", |input: &mut Input<'s>| {
                        oper_begin.parse_next(input)
                    }),
                    or_parse_error(
                        |input: &mut Input<'s>| inner.parse_next(input),
                        ParseErrorType::Expected("expression"),
                    ),
                    trace("oper_end", |input: &mut Input<'s>| {
                        oper_end.parse_next(input)
                    }),
                ),
            )
            .fold(
                move || init.clone(),
                |acc,
                 (op_begin, val, _op_end): (
                    <Input as Stream>::Slice,
                    Expr,
                    <Input as Stream>::Slice,
                )| {
                    Expr::Binary(
                        Binary::from_oper(op_begin, acc, val).expect("all operators are correct"),
                    )
                },
            )
            .parse_next(input)?;
            Ok(v)
        }
    }

    pub fn logical_or<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::logical_and, "or", "").parse_next(input)
    }

    pub fn logical_and<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::equality, "and", "").parse_next(input)
    }

    pub fn equality<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::comparison, alt(("==", "!=")), "").parse_next(input)
    }

    pub fn comparison<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::term, alt(("<=", ">=", "<", ">")), "").parse_next(input)
    }

    pub fn term<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::factor, one_of(['+', '-']).take(), "").parse_next(input)
    }

    pub fn factor<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::unary, one_of(['*', '/']).take(), "").parse_next(input)
    }

    pub fn unary<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
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
                (space_wrap(operator), space_wrap(Expr::unary)).map(|(neg, e)| match neg {
                    Sign::Negate => Expr::Unary(Unary::Negate(Rc::new(e))),
                    Sign::Not => Expr::Unary(Unary::Not(Rc::new(e))),
                }),
            ),
            trace("no unary", Expr::call),
        ))
        .parse_next(input)
    }

    pub fn call<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        let init = Expr::primary(input)?;
        trace(
            "function calls",
            repeat(0.., delimited("(", Expr::arguments, space_wrap(")"))).fold(
                move || init.clone(),
                |acc, args: Vec<Expr>| Expr::FnCall(Rc::new(acc), args.into()),
            ),
        )
        .parse_next(input)
    }

    fn arguments<'s>(input: &mut Input<'s>) -> ModalResult<Vec<Expr>, Error<Input<'s>>> {
        let head = trace("first argument", opt(Expr::parser)).parse_next(input)?;
        let Some(head) = head else {
            return Ok(vec![]);
        };

        let rest: Vec<Expr> = trace(
            "rest of arguments",
            repeat(0.., preceded(space_wrap(","), Expr::parser)),
        )
        .parse_next(input)?;
        Ok(once(head).chain(rest).collect::<Vec<Expr>>())
    }

    pub fn primary<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "primary",
            space_wrap(alt((
                Expr::parenthesis,
                Self::variable,
                Literal::parser.map(Expr::Literal),
            ))),
        )
        .parse_next(input)
    }

    fn parenthesis<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "parenthesis",
            preceded(
                space_wrap("("),
                terminated(
                    Self::parser,
                    or_parse_error(space_wrap(")"), ParseErrorType::Expected("expression")),
                ),
            ),
        )
        .map(|e| Self::Group(Rc::new(e)))
        .parse_next(input)
    }

    pub(crate) fn word<'s>(input: &mut Input<'s>) -> ModalResult<String, Error<Input<'s>>> {
        trace(
            "word",
            (
                any.verify(|c: &<Input as Stream>::Token| {
                    let c = c.as_char();
                    c.is_alphabetic() || c == '_'
                }),
                take_while(0.., |c: <Input as Stream>::Token| {
                    let c = c.as_char();
                    c.is_alphanumeric() || c == '_'
                }),
            )
                .take()
                .map(ToString::to_string),
        )
        .parse_next(input)
    }

    fn variable<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        let name = Self::identifier.parse_next(input)?;
        if input.state.is_declared(&name) {
            return Err(ErrMode::Cut(Error::from(
                EvaluateError::CannotAssignToSelf(name),
            )));
        }
        let depth = input.state.depth(&name);
        log::debug!("Found variable [{name}] with depth {depth:?}");

        Ok(Expr::Variable(name, depth))
    }

    pub(crate) fn identifier<'s>(input: &mut Input<'s>) -> ModalResult<String, Error<Input<'s>>> {
        let id = trace("identifier", Self::word).parse_next(input)?;
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

#[derive(Debug, Clone, PartialEq)]
pub enum Unary {
    Negate(Rc<Expr>),
    Not(Rc<Expr>),
}

impl Evaluate for Unary {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        match self {
            Self::Negate(e) => Ok(match e.evaluate(env)? {
                Value::Number(n) => Value::Number(-n),
                _ => {
                    return Err(EvaluateError::TypeMismatch {
                        expected: "number".into(),
                    });
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
            Self::Negate(e) => write!(f, "(- {e})"),
            Self::Not(e) => write!(f, "(! {e})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binary {
    Mul(Rc<Expr>, Rc<Expr>),
    Div(Rc<Expr>, Rc<Expr>),
    Add(Rc<Expr>, Rc<Expr>),
    Sub(Rc<Expr>, Rc<Expr>),
    LessThan(Rc<Expr>, Rc<Expr>),
    GreaterThan(Rc<Expr>, Rc<Expr>),
    LessEq(Rc<Expr>, Rc<Expr>),
    GreaterEq(Rc<Expr>, Rc<Expr>),
    Equals(Rc<Expr>, Rc<Expr>),
    NotEquals(Rc<Expr>, Rc<Expr>),
    Or(Rc<Expr>, Rc<Expr>),
    And(Rc<Expr>, Rc<Expr>),
}

impl Evaluate for Binary {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        match self {
            Self::Mul(l, r) => Self::eval_mul(l, env, r),
            Self::Div(l, r) => Self::eval_div(l, env, r),
            Self::Add(l, r) => Self::eval_add(l, env, r),
            Self::Sub(l, r) => Self::eval_sub(l, env, r),
            Self::LessThan(l, r) => Self::eval_less_than(l, env, r),
            Self::GreaterThan(l, r) => Self::eval_greater_than(l, env, r),
            Self::LessEq(l, r) => Self::eval_less_eq(l, env, r),
            Self::GreaterEq(l, r) => Self::eval_greater_eq(l, env, r),
            Self::Equals(l, r) => Self::eval_equals(l, env, r),
            Self::NotEquals(l, r) => Self::eval_not_equals(l, env, r),
            Self::Or(l, r) => Self::eval_or(l, env, r),
            Self::And(l, r) => Self::eval_and(l, env, r),
        }
    }
}

impl Binary {
    fn eval_div(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l / r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_mul(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l * r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_sub(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l - r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_add(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l + r),
            (Value::String(s), Value::String(t)) => Value::from(format!("{s}{t}")),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "two numbers or two strings".into(),
                });
            }
        })
    }

    fn eval_greater_eq(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l >= r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_less_eq(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l <= r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_greater_than(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l > r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_less_than(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from(l < r),
            _ => {
                return Err(EvaluateError::TypeMismatch {
                    expected: "numbers".into(),
                });
            }
        })
    }

    fn eval_and(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
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

    fn eval_or(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
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

    fn eval_not_equals(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from((l - r).abs() > F64_PRECISION),
            (Value::String(s), Value::String(t)) => Value::from(s != t),
            (Value::Nil, Value::Nil) => Value::Bool(false),
            (Value::Bool(l), Value::Bool(r)) if l == r => Value::Bool(false),
            _ => Value::Bool(true),
        })
    }

    fn eval_equals(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => Value::from((l - r).abs() < F64_PRECISION),
            (Value::String(s), Value::String(t)) => Value::from(s == t),
            (Value::Nil, Value::Nil) => Value::Bool(true),
            (Value::Bool(l), Value::Bool(r)) if l == r => Value::Bool(true),
            _ => Value::Bool(false),
        })
    }

    fn from_oper<'s>(op_begin: &str, l: Expr, r: Expr) -> Result<Binary, Error<Input<'s>>> {
        let l = Rc::new(l);
        let r = Rc::new(r);
        Ok(match op_begin {
            "or" => Binary::Or(l, r),
            "and" => Binary::And(l, r),
            "==" => Binary::Equals(l, r),
            "!=" => Binary::NotEquals(l, r),
            ">=" => Binary::GreaterEq(l, r),
            "<=" => Binary::LessEq(l, r),
            ">" => Binary::GreaterThan(l, r),
            "<" => Binary::LessThan(l, r),
            "+" => Binary::Add(l, r),
            "-" => Binary::Sub(l, r),
            "*" => Binary::Mul(l, r),
            "/" => Binary::Div(l, r),
            _ => {
                return Err(Error::Parse(ParseError::new(
                    ParseErrorType::InvalidOperator,
                    op_begin,
                    &Stateful::new("", State::default()),
                )));
            }
        })
    }
}

impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mul(l, r) => write!(f, "(* {l} {r})"),
            Self::Div(l, r) => write!(f, "(/ {l} {r})"),
            Self::Add(l, r) => write!(f, "(+ {l} {r})"),
            Self::Sub(l, r) => write!(f, "(- {l} {r})"),
            Self::LessThan(l, r) => write!(f, "(< {l} {r})"),
            Self::GreaterThan(l, r) => write!(f, "(> {l} {r})"),
            Self::LessEq(l, r) => write!(f, "(<= {l} {r})"),
            Self::GreaterEq(l, r) => write!(f, "(>= {l} {r})"),
            Self::Equals(l, r) => write!(f, "(== {l} {r})"),
            Self::NotEquals(l, r) => write!(f, "(!= {l} {r})"),
            Self::Or(l, r) => write!(f, "(or {l} {r})"),
            Self::And(l, r) => write!(f, "(and {l} {r})"),
        }
    }
}

#[derive(Clone)]
pub enum Literal {
    True,
    False,
    Nil,
    Number(f64),
    String(Rc<str>),
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::Nil => write!(f, "Nil"),
            Self::Number(n) => f.debug_tuple("Number").field(n).finish(),
            Self::String(s) => f.debug_tuple("String").field(s).finish(),
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        if b { Self::True } else { Self::False }
    }
}

impl From<f64> for Literal {
    fn from(n: f64) -> Self {
        Self::Number(n)
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        Self::String(s.into())
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Self::String(s.into())
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
    fn evaluate<'s, 'ctx>(&'s self, _env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        Ok(match self {
            Self::Nil => Value::Nil,
            Self::True => Value::Bool(true),
            Self::False => Value::Bool(false),
            Self::Number(n) => Value::Number(*n),
            Self::String(s) => Value::String(s.clone()),
        })
    }
}

impl Literal {
    pub(crate) fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "literal",
            space_wrap(alt((
                "true".value(Self::True),
                "false".value(Self::False),
                "nil".value(Self::Nil),
                Self::number,
                Self::string,
            ))),
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
        .map(|s: S::Slice| Self::String(std::str::from_utf8(s.as_bstr()).unwrap().into()))
        .parse_next(input)
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
        }
    }
}

pub fn parse(input: &str) -> Result<Expr, Error<Input<'_>>> {
    Ok(Expr::parser.parse(Stateful::new(input, State::new(1)))?)
}
