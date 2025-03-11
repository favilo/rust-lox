use either::Either;
use std::{
    hash::Hash,
    iter::{once, zip},
    rc::Rc,
};
use winnow::{
    ModalResult, Parser,
    ascii::{Caseless, digit1},
    combinator::{alt, delimited, empty, fail, not, opt, preceded, repeat, terminated, trace},
    dispatch,
    error::{ErrMode, ParserError},
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    token::{any, one_of, take_till, take_while},
};

use crate::{
    error::{Error, EvaluateError, ParseError, ParseErrorType},
    interpreter::Context,
    parser::{
        Callable, NativeCallable,
        ast::Statement,
        state::{State, Stateful},
    },
};

use super::{Evaluate, Input, Instance, NativeFn, Value, or_parse_error, space_wrap};

#[cfg(test)]
mod tests;

const F64_PRECISION: f64 = 1e-10;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable { name: String, depth: Option<usize> },
    Group(Rc<Expr>),
    Unary(Unary),
    Binary(Binary),
    Assignment(Assignment),
    FnCall(FnCall),
    GetProperty(GetProperty),
    SetProperty(SetProperty),
    This { depth: Option<usize> },
}

impl Evaluate for Expr {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        match self {
            Self::Literal(lit) => lit.evaluate(env),
            Self::Variable { name, depth } => {
                log::trace!("Getting variable [{name}] with depth {depth:?}");
                env.get(name, *depth)
            }
            Self::Group(block) => block.evaluate(env),
            Self::Unary(unary) => unary.evaluate(env),
            Self::Binary(binary) => binary.evaluate(env),
            Self::Assignment(assign) => assign.evaluate(env),
            Self::FnCall(fn_call) => fn_call.evaluate(env),
            Self::GetProperty(prop) => prop.evaluate(env),
            Self::SetProperty(prop) => prop.evaluate(env),
            Self::This { depth } => env
                .get("this", depth.to_owned())
                .map_err(|_| EvaluateError::ThisOutsideClass),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Literal(Literal::Number(n)) => write!(f, "{n:?}"),
            Self::Literal(l) => write!(f, "{l}"),
            Self::Variable { name, .. } => write!(f, "{name}"),
            Self::Unary(u) => write!(f, "{u}"),
            Self::Binary(b) => write!(f, "{b}"),
            Self::Group(e) => write!(f, "(group {e})"),
            Self::Assignment(Assignment {
                variable_name,
                value: e,
                ..
            }) => write!(f, "(= {variable_name} {e})"),
            Self::FnCall(FnCall {
                fn_expr: expr,
                params: args,
            }) => {
                write!(
                    f,
                    "({} {})",
                    expr,
                    args.iter().map(ToString::to_string).collect::<String>()
                )
            }
            Self::GetProperty(GetProperty { inst, field }) => write!(f, "(. {inst} {field})"),
            Self::SetProperty(SetProperty { inst, field, value }) => {
                write!(f, "(= (. {inst} {field}) {value})")
            }
            Self::This { .. } => write!(f, "this"),
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
        trace("assignment", move |input: &mut Input<'s>| {
            let get_expr = Expr::call.parse_next(input)?;
            space_wrap(terminated("=", not("="))).parse_next(input)?;
            let value = Expr::parser.parse_next(input)?;
            log::trace!("Assignment: {get_expr:?} = {value}");
            match get_expr {
                Expr::GetProperty(property) => {
                    let set_property = Expr::SetProperty(SetProperty {
                        inst: property.inst,
                        field: property.field,
                        value: Rc::new(value),
                    });
                    Ok(set_property)
                }
                Expr::Variable { name, depth } => {
                    log::debug!("Assigning to variable [{name}] with depth {depth:?}");
                    Ok(Expr::Assignment(Assignment {
                        variable_name: name.into(),
                        depth,
                        value: Rc::new(value),
                    }))
                }
                _ => Err(ErrMode::Cut(Error::from(ParseError::new(
                    ParseErrorType::InvalidAssignment,
                    "=",
                    input,
                )))),
            }
        })
        .parse_next(input)
    }

    fn generic_oper<'s, Ex, Op>(
        mut inner: Ex,
        mut oper_begin: Op,
    ) -> impl Parser<Input<'s>, Expr, ErrMode<Error<Input<'s>>>>
    where
        Ex: Parser<Input<'s>, Expr, ErrMode<Error<Input<'s>>>>,
        Op: Parser<Input<'s>, <Input<'s> as Stream>::Slice, ErrMode<Error<Input<'s>>>>,
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
                ),
            )
            .fold(
                move || init.clone(),
                |acc, (op_begin, val): (<Input as Stream>::Slice, Expr)| {
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
        Self::generic_oper(Expr::logical_and, space_wrap("or")).parse_next(input)
    }

    pub fn logical_and<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::equality, space_wrap("and")).parse_next(input)
    }

    pub fn equality<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::comparison, space_wrap(alt(("==", "!=")))).parse_next(input)
    }

    pub fn comparison<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::term, space_wrap(alt(("<=", ">=", "<", ">")))).parse_next(input)
    }

    pub fn term<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::factor, one_of(['+', '-']).take()).parse_next(input)
    }

    pub fn factor<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        Self::generic_oper(Expr::unary, one_of(['*', '/']).take()).parse_next(input)
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
            trace("no unary - call", Expr::call),
        ))
        .parse_next(input)
    }

    pub fn call<'s>(input: &mut Input<'s>) -> ModalResult<Expr, Error<Input<'s>>> {
        let init = Expr::primary(input)?;
        let fn_call = delimited(
            "(",
            trace("call arguments", Expr::arguments),
            or_parse_error(space_wrap(")"), ParseErrorType::Expected(")")),
        )
        .map(Either::Left);
        let dot_property = preceded(
            space_wrap("."),
            or_parse_error(
                Self::identifier,
                ParseErrorType::Expected("property name after '.'"),
            ),
        )
        .map(Either::Right);
        trace(
            "calls",
            repeat(0.., alt((fn_call, dot_property))).fold(
                move || init.clone(),
                |acc, next: Either<Vec<Expr>, String>| match next {
                    Either::Left(args) => Expr::FnCall(FnCall {
                        fn_expr: Rc::new(acc),
                        params: args.into(),
                    }),
                    Either::Right(name) => Expr::GetProperty(GetProperty {
                        inst: Rc::new(acc),
                        field: name.into(),
                    }),
                },
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
        if rest.len() >= 255 {
            Err(ErrMode::Cut(Error::from(ParseError::new(
                ParseErrorType::TooManyArguments,
                "",
                input,
            ))))
        } else {
            Ok(once(head).chain(rest).collect::<Vec<Expr>>())
        }
    }

    pub fn primary<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "primary",
            space_wrap(alt((
                Expr::parenthesis,
                Self::variable,
                Literal::parser.map(Expr::Literal),
                Self::this,
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

    fn this<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        space_wrap("this").parse_next(input)?;
        let depth = input.state.depth("this");
        log::debug!("Found [this] with depth {depth:?}");
        Ok(Expr::This { depth })
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

        Ok(Expr::Variable { name, depth })
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
            (Value::Number(l), Value::Number(r)) => {
                if l.is_nan() || r.is_nan() {
                    return Ok(Value::Bool(true));
                }
                Value::from((l - r).abs() > F64_PRECISION)
            }
            (Value::String(s), Value::String(t)) => Value::from(s != t),
            (Value::Nil, Value::Nil) => Value::Bool(false),
            (Value::Bool(l), Value::Bool(r)) if l == r => Value::Bool(false),
            _ => Value::Bool(true),
        })
    }

    fn eval_equals(l: &Expr, env: &Context, r: &Expr) -> Result<Value, EvaluateError> {
        Ok(match (l.evaluate(env)?, r.evaluate(env)?) {
            (Value::Number(l), Value::Number(r)) => {
                if l.is_nan() || r.is_nan() {
                    return Ok(Value::Bool(false));
                }
                Value::from((l - r).abs() < F64_PRECISION)
            }
            (Value::String(s), Value::String(t)) => Value::from(s == t),
            (Value::Nil, Value::Nil) => Value::Bool(true),
            (Value::Bool(l), Value::Bool(r)) if l == r => Value::Bool(true),
            (Value::Class(l), Value::Class(r)) => Value::Bool(Rc::ptr_eq(&l, &r)),
            (Value::Callable(l), Value::Callable(r)) => Value::Bool(l == r),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub variable_name: Rc<str>,
    pub depth: Option<usize>,
    pub value: Rc<Expr>,
}

impl Evaluate for Assignment {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        let value = self.value.evaluate(env)?;
        let Assignment {
            variable_name,
            depth,
            ..
        } = self;
        log::trace!("Setting variable [{variable_name}] to {value} with depth {depth:?}");
        env.set(variable_name, value.clone(), *depth)?;
        Ok(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    pub fn_expr: Rc<Expr>,
    pub params: Rc<[Expr]>,
}

impl Evaluate for FnCall {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        let FnCall { fn_expr, params } = self;
        log::trace!("Function call: {fn_expr}({params:#?})");
        let param_values = params
            .iter()
            .map(|e| e.evaluate(env))
            .collect::<Result<Vec<_>, _>>()?;
        let callable = fn_expr.evaluate(env)?;
        log::trace!("Callable: {callable}");
        match callable {
            Value::NativeCallable(NativeCallable { arg_num: n, func }) => {
                Self::eval_native_fn(&param_values, n, &func, env)
            }
            Value::Callable(callable) => Self::eval_callable(&callable, &param_values, env),
            Value::Class(class) => class.eval_class_call(&param_values, env),

            _ => Err(EvaluateError::NotCallable(callable)),
        }
    }
}

impl FnCall {
    fn eval_native_fn(
        args: &[Value],
        n: usize,
        f: &NativeFn,
        ctx: &Context,
    ) -> Result<Value, EvaluateError> {
        // TODO: Add currying
        if n != args.len() {
            return Err(EvaluateError::ArgumentMismatch {
                expected: n,
                got: args.len(),
            });
        }
        Ok(f(args, ctx))
    }

    pub(crate) fn eval_callable(
        callable: &Callable,
        param_values: &[Value],
        env: &Context,
    ) -> Result<Value, EvaluateError> {
        // TODO: Add currying
        if env.stack_depth() > 133 {
            return Err(EvaluateError::StackOverflow);
        }
        if callable.param_names.len() != param_values.len() {
            return Err(EvaluateError::ArgumentMismatch {
                expected: callable.param_names.len(),
                got: param_values.len(),
            });
        }
        let fn_env = callable.env.child();
        zip(callable.param_names.iter(), param_values.iter())
            .try_for_each(|(param, value)| fn_env.declare(param, value.clone(), Some(0)))?;
        log::trace!("Function env: {fn_env:#?}");
        let stmts = match callable.body.as_ref() {
            Statement::Block(stmts) => stmts.clone(),
            _ => {
                return Err(EvaluateError::FunctionBodyNotBlock(
                    callable.body.to_string(),
                ));
            }
        };
        env.push_stack();
        let result = stmts
            .iter()
            .try_fold(Value::Nil, |_, stmt| stmt.evaluate(&fn_env));
        env.pop_stack();
        match (callable.initializer, result) {
            (true, Ok(_) | Err(EvaluateError::Return(Value::Nil))) => {
                log::trace!("Initializer returning 'this': {fn_env:#?}");
                let this = fn_env.get("this", Some(1));
                assert!(this.is_ok(), "No 'this' in initializer");
                this
            }
            (true, Err(EvaluateError::Return(_))) => {
                panic!("Returning from initializer should have been a parse error")
            }
            (false, Ok(r) | Err(EvaluateError::Return(r))) => Ok(r),
            (_, e) => e,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetProperty {
    pub inst: Rc<Expr>,
    pub field: Rc<str>,
}

impl Evaluate for GetProperty {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        let GetProperty { inst, field } = self;
        let instance = inst.evaluate(env)?;
        match instance {
            Value::Instance(Instance { fields, .. }) => {
                let fields_borrow = fields.borrow();
                let Some(value) = fields_borrow.get(field).cloned() else {
                    return Err(EvaluateError::UndefinedProperty(field.to_string()));
                };
                Ok(value)
            }
            _ => Err(EvaluateError::TypeMismatch {
                expected: "instance".into(),
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetProperty {
    inst: Rc<Expr>,
    field: Rc<str>,
    value: Rc<Expr>,
}

impl Evaluate for SetProperty {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        let SetProperty { inst, field, value } = self;
        let instance = inst.evaluate(env)?;
        match instance {
            Value::Instance(Instance { fields, .. }) => {
                let value = value.evaluate(env)?;
                fields.borrow_mut().insert(field.clone(), value.clone());

                Ok(value)
            }
            _ => Err(EvaluateError::TypeMismatch {
                expected: "instance".into(),
            }),
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

    fn string<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "string",
            preceded(
                "\"",
                or_parse_error(
                    terminated(take_till(0.., '"'), "\""),
                    ParseErrorType::UnterminatedString,
                ),
            ),
        )
        .map(|s: <Input as Stream>::Slice| Self::String(s.into()))
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
