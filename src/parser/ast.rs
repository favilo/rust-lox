use std::{iter::once, rc::Rc};

use winnow::{
    ModalResult, Parser,
    combinator::{alt, opt, preceded, repeat, terminated, trace},
    error::ErrMode,
    seq,
};

use crate::{
    error::{Error, EvaluateError, ParseError, ParseErrorType},
    interpreter::Context,
    parser::{Callable, Class, full_word, or_parse_error, space_wrap},
};

use super::{
    Evaluate, Input, Run, Stateful, Value,
    expr::{Expr, Literal},
    state::State,
};

#[cfg(test)]
mod tests;

const BLOCK_LIMIT: usize = 32767;

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    statements: Vec<Statement>,
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{stmt}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
    Var(Rc<str>, Option<usize>, Expr),
    Block(Rc<[Statement]>),
    If(Expr, Rc<Statement>, Option<Rc<Statement>>),
    While(Expr, Rc<Statement>),
    For(For),
    Function(Function),
    Return(Expr),
    Class {
        name: Rc<str>,
        methods: Rc<[Statement]>,
    },
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => write!(f, "{expr};"),
            Statement::Print(expr) => write!(f, "print {expr};"),
            Statement::Var(_, _depth, expr) => write!(f, "var = {expr};"),
            Statement::Block(stmts) => {
                writeln!(f, "{{")?;
                for stmt in stmts.iter() {
                    writeln!(f, "{stmt}")?;
                }
                write!(f, "}}")
            }
            Statement::If(expr, stmt, statement1) => write!(
                f,
                "if ({expr}) {stmt}{}",
                if let Some(else_stmt) = statement1 {
                    format!(" else {else_stmt}")
                } else {
                    String::new()
                }
            ),
            Statement::While(expr, stmt) => write!(f, "while ({expr}) {stmt}"),
            Statement::For(for_expr) => write!(f, "{for_expr}",),
            Statement::Function(func) => {
                write!(f, "{func}")
            }
            Statement::Return(expr) => write!(f, "return {expr};"),
            Statement::Class { name, methods } => {
                writeln!(f, "class {name} {{")?;
                for method in methods.iter() {
                    writeln!(f, "{method:?}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Evaluate for Ast {
    fn evaluate<'s, 'ctx>(&'s self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        let mut last = Value::Nil;
        for statement in &self.statements {
            let result = statement.evaluate(env);
            if let Err(EvaluateError::Return(value)) = result {
                return Ok(value);
            }
            last = result?;
        }

        Ok(last)
    }
}

impl Evaluate for Statement {
    fn evaluate<'s, 'ctx>(&self, env: &'ctx Context) -> Result<Value, EvaluateError>
    where
        's: 'ctx,
    {
        match self {
            Statement::Expr(expr) => {
                log::trace!("evaluate: {}", expr);
                expr.evaluate(env)
            }
            Statement::Print(expr) => {
                log::trace!("print: {}", expr);
                env.print(&expr.evaluate(env)?);
                Ok(Value::Nil)
            }
            Statement::Var(name, depth, expr) => {
                log::trace!("var: {} = {}", name, expr);
                let value = expr.evaluate(env)?;
                log::trace!("Declaring variable [{name}] to {value} with depth {depth:?}");
                env.declare(name, value.clone(), *depth)?;
                log::debug!("define: {} = {:?}", name, value);
                log::debug!("environment: {:?}", env);
                Ok(value)
            }
            Statement::Block(stmts) => {
                log::trace!("block: {:?}", stmts);
                let mut last = Value::Nil;
                let block_env = env.child();
                log::debug!("block environment: {:?}", block_env);
                for stmt in stmts.iter() {
                    let result = stmt.evaluate(&block_env);
                    last = result?;
                }
                Ok(last)
            }
            Statement::If(condition, true_s, false_s) => {
                log::trace!("if: {} {} {}", condition, true_s, false_s.is_some());
                let condition = bool::from(condition.evaluate(env)?);
                if condition {
                    true_s.evaluate(env)
                } else if let Some(false_s) = false_s {
                    false_s.evaluate(env)
                } else {
                    Ok(Value::Nil)
                }
            }
            Statement::While(condition, stmt) => {
                log::trace!("while: {} {}", condition, stmt);
                let mut last = Value::Nil;
                while bool::from(condition.evaluate(env)?) {
                    last = stmt.evaluate(env)?;
                }
                Ok(last)
            }
            Statement::For(For {
                init,
                condition,
                increment,
                body,
            }) => Self::eval_for(env, init.as_ref(), condition, increment.as_ref(), body),
            Statement::Return(expr) => {
                log::trace!("return: {}", expr);
                let depth = env.depth();
                if depth.is_none() {
                    return Err(EvaluateError::TopLevelReturn);
                }
                Err(EvaluateError::Return(expr.evaluate(env)?))
            }
            Statement::Function(func) => Self::eval_func(env, func),
            Statement::Class { name, methods } => Self::eval_class(env, name, methods),
        }
    }
}

impl Statement {
    fn eval_for(
        env: &Context,
        init: Option<&Rc<Statement>>,
        condition: &Expr,
        increment: Option<&Expr>,
        body: &Rc<Statement>,
    ) -> Result<Value, EvaluateError> {
        log::trace!("for: {:?} {:?} {:?} {:?}", init, condition, increment, body);
        let for_env = env.child();
        let mut last = Value::Nil;
        if let Some(init) = init {
            init.evaluate(&for_env)?;
        }
        while bool::from(condition.evaluate(&for_env)?) {
            last = body.evaluate(&for_env)?;
            if let Some(increment) = increment {
                increment.evaluate(&for_env)?;
            }
        }
        Ok(last)
    }

    fn eval_func(env: &Context, func: &Function) -> Result<Value, EvaluateError> {
        let Function {
            name,
            param_names,
            body,
            initializer,
        } = func;
        log::trace!("function: {name}({}) {body}", param_names.join(", "));
        if !matches!(body.as_ref(), Statement::Block(_)) {
            return Err(EvaluateError::FunctionBodyNotBlock(name.to_string()));
        }
        let value = Value::Callable(Callable {
            name: name.clone(),
            param_names: param_names.clone(),
            body: body.clone(),
            env: env.clone(),
            initializer: *initializer,
        });
        let depth = env.depth();
        log::debug!("define: {name} = {value:#?} with depth {depth:?}");
        env.declare(name, value.clone(), depth)?;
        log::debug!("function definition global env: {env:?}");

        Ok(value)
    }

    fn eval_class(
        env: &Context,
        name: &Rc<str>,
        methods: &Rc<[Statement]>,
    ) -> Result<Value, EvaluateError> {
        log::trace!("class: {name} {{ {methods:?} }}");
        let class_env = env.child();
        let methods = methods
            .iter()
            .map(|method| {
                if let Statement::Function(Function {
                    name,
                    param_names,
                    body,
                    ..
                }) = method
                {
                    let value = Value::Callable(Callable {
                        name: name.clone(),
                        param_names: param_names.clone(),
                        body: body.clone(),
                        env: class_env.clone(),
                        initializer: name.as_ref() == "init",
                    });
                    Ok(value)
                } else {
                    Err(EvaluateError::ClassMethodNotFunction(name.to_string()))
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value = Value::Class(Rc::new(Class {
            name: name.clone(),
            methods: methods.into(),
            context: env.clone(),
        }));
        let depth = env.depth();
        log::debug!("define: {name} = {value:#?} with depth {depth:?}");
        env.declare(name, value.clone(), depth)?;
        log::debug!("class definition global env: {env:?}");

        Ok(value)
    }
}

impl Run for Ast {
    fn run(&self) -> Result<(), EvaluateError> {
        let env = Context::new();
        log::trace!("*** Begin running ***");
        self.evaluate(&env)?;
        log::trace!("*** Done running ***\n");
        Ok(())
    }
}

impl Ast {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace(
            "ast",
            space_wrap(repeat(0.., Statement::parser)).map(|statements| Self { statements }),
        )
        .parse_next(input)
    }
}

impl Statement {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        trace("statement", space_wrap(Self::declaration)).parse_next(input)
    }

    fn declaration<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        alt((
            Self::class_decl,
            Self::func_decl,
            Self::var_decl,
            Self::stmt,
        ))
        .parse_next(input)
    }

    fn class_decl<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        trace("class declaration", move |input: &mut Input<'s>| {
            full_word("class").parse_next(input)?;
            let name = or_parse_error(Expr::identifier, ParseErrorType::Expected("class name"))
                .parse_next(input)?;
            input
                .state
                .declare(&name)
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;

            input
                .state
                .define(&name)
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;

            or_parse_error(space_wrap("{"), ParseErrorType::Expected("'{'")).parse_next(input)?;
            input.state.start_scope();
            input
                .state
                .declare("this")
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;

            input
                .state
                .define("this")
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;

            let mut methods: Vec<Statement> = repeat(0.., Self::function).parse_next(input)?;
            for m in &mut methods {
                let Statement::Function(f) = m else {
                    continue;
                };
                if f.name.as_ref() == "init" {
                    f.initializer = true;
                    let Statement::Block(body) = f.body.as_ref() else {
                        return Err(ErrMode::Cut(Error::from(
                            EvaluateError::FunctionBodyNotBlock(f.name.to_string()),
                        )));
                    };
                    if body
                        .iter()
                        .filter(|stmt| {
                            let Statement::Return(r) = stmt else {
                                return false;
                            };
                            r != &Expr::Literal(Literal::Nil)
                        })
                        .count()
                        > 0
                    {
                        return Err(ErrMode::Cut(Error::from(
                            EvaluateError::ReturnFromInitializer,
                        )));
                    }
                }
            }
            or_parse_error(space_wrap("}"), ParseErrorType::Expected("'}'")).parse_next(input)?;
            input.state.end_scope();
            Ok(Statement::Class {
                name: name.into(),
                methods: methods.into(),
            })
        })
        .parse_next(input)
    }

    fn func_decl<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        trace("fun_decl", move |input: &mut Input<'s>| {
            full_word("fun").parse_next(input)?;
            Self::function.parse_next(input)
        })
        .parse_next(input)
    }

    fn function<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        trace("function/method", move |input: &mut Input<'s>| {
            let name = Expr::identifier.parse_next(input)?;
            input
                .state
                .declare(&name)
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;

            input
                .state
                .define(&name)
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;

            or_parse_error(space_wrap("("), ParseErrorType::Expected("'('")).parse_next(input)?;
            let params = Self::parameter_names.parse_next(input)?;
            input.state.start_scope();
            log::trace!(
                "Entering function params scope: {:?}",
                input.state.scopes().collect::<Vec<_>>()
            );
            params
                .iter()
                .try_for_each(|param| {
                    input.state.declare(param)?;
                    input.state.define(param)
                })
                .map_err(|e| ErrMode::Cut(Error::from(e)))?;
            or_parse_error(
                space_wrap(")"),
                ParseErrorType::Expected("')' after parameters"),
            )
            .parse_next(input)?;
            or_parse_error(space_wrap("{"), ParseErrorType::Expected("'{'")).parse_next(input)?;
            // Don't just use `Statement::block` because we don't want the extra scope
            let body = Ast::parser.parse_next(input)?;
            if body.statements.len() > BLOCK_LIMIT {
                return Err(ErrMode::Cut(Error::from(EvaluateError::TooLargeBody(
                    body.statements.len(),
                ))));
            }
            or_parse_error(space_wrap("}"), ParseErrorType::Expected("'}'")).parse_next(input)?;
            input.state.end_scope();
            log::debug!("Exiting function scope: {}", input.state.scope_len());
            Ok(Statement::Function(Function {
                name: name.into(),
                param_names: params.into(),
                body: Statement::Block(body.statements.into()).into(),
                initializer: false,
            }))
        })
        .parse_next(input)
    }

    fn parameter_names<'s>(input: &mut Input<'s>) -> ModalResult<Vec<Rc<str>>, Error<Input<'s>>> {
        trace("parameter names", move |input: &mut Input<'s>| {
            let head = trace("first argument", opt(Expr::identifier)).parse_next(input)?;
            let Some(head) = head else {
                return Ok(vec![]);
            };

            let rest: Vec<String> = trace(
                "rest of arguments",
                repeat(0.., preceded(space_wrap(","), Expr::identifier)),
            )
            .parse_next(input)?;
            if rest.len() >= 255 {
                Err(ErrMode::Cut(Error::from(ParseError::new(
                    ParseErrorType::TooManyParameters,
                    "",
                    input,
                ))))
            } else {
                Ok(once(head).chain(rest).map(Into::into).collect::<Vec<_>>())
            }
        })
        .parse_next(input)
    }

    fn stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        space_wrap(alt((
            Self::block,
            Self::condition,
            Self::while_loop,
            Self::for_loop,
            Self::print,
            Self::return_stmt,
            Self::expr_stmt,
        )))
        .parse_next(input)
    }

    fn for_loop<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        (
            full_word("for"),
            space_wrap(or_parse_error("(", ParseErrorType::Expected("'('"))),
        )
            .parse_next(input)?;
        input.state.start_scope();
        let init = opt(alt((Self::var_decl, Self::expr_stmt))).parse_next(input)?;
        if init.is_none() {
            space_wrap(or_parse_error(";", ParseErrorType::Expected("expression")))
                .parse_next(input)?;
        }
        let condition = opt(Expr::parser).parse_next(input)?;
        space_wrap(or_parse_error(";", ParseErrorType::Expected("expression")))
            .parse_next(input)?;
        let increment = opt(Expr::parser).parse_next(input)?;
        space_wrap(or_parse_error(")", ParseErrorType::Expected("expression")))
            .parse_next(input)?;
        let body =
            or_parse_error(Self::stmt, ParseErrorType::Expected("expression")).parse_next(input)?;
        input.state.end_scope();
        Ok(Statement::For(For {
            init: init.map(Rc::new),
            condition: condition.unwrap_or(Expr::Literal(Literal::from(true))),
            increment,
            body: Rc::new(body),
        }))
    }

    fn while_loop<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        (
            full_word("while"),
            space_wrap(or_parse_error("(", ParseErrorType::Expected("'('"))),
        )
            .parse_next(input)?;
        seq! {
            or_parse_error(Expr::parser, ParseErrorType::Expected("expression")),
            _: (space_wrap(or_parse_error(")", ParseErrorType::Expected("')'")))),
            or_parse_error(Self::stmt, ParseErrorType::Expected("expression")),
        }
        .map(|(condition, stmt)| Statement::While(condition, Rc::new(stmt)))
        .parse_next(input)
    }

    fn condition<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        (
            space_wrap(full_word("if")),
            space_wrap(or_parse_error("(", ParseErrorType::Expected("'('"))),
        )
            .parse_next(input)?;
        seq! {
            Expr::parser,
            _: space_wrap(or_parse_error(")", ParseErrorType::Expected("')'"))),
            or_parse_error(Self::stmt, ParseErrorType::Expected("expression")),
            opt(preceded(full_word("else"), or_parse_error(Self::stmt, ParseErrorType::Expected("expression")))),
        }
        .map(|(condition, true_s, false_s)| {
            Statement::If(condition, Rc::new(true_s), false_s.map(Rc::new))
        })
        .parse_next(input)
    }

    fn block<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        "{".parse_next(input)?;
        input.state.start_scope();
        log::debug!(
            "Entering block scope: {:?}",
            input.state.scopes().collect::<Vec<_>>()
        );
        let ast = Ast::parser.parse_next(input)?;
        let block = {
            if ast.statements.len() > BLOCK_LIMIT {
                return Err(ErrMode::Cut(Error::from(EvaluateError::TooLargeBody(
                    ast.statements.len(),
                ))));
            }
            Ok(Statement::Block(ast.statements.into()))
        }?;
        log::debug!("Exiting block scope: {}", input.state.scope_len());
        input.state.end_scope();
        or_parse_error(space_wrap("}"), ParseErrorType::Expected("'}'")).parse_next(input)?;
        Ok(block)
    }

    fn var_decl<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        full_word("var").parse_next(input)?;
        let (id, var) = seq! {
            or_parse_error(Expr::identifier, ParseErrorType::Expected("variable name")),
            opt(space_wrap("=").void()),
        }
        .parse_next(input)?;
        input
            .state
            .declare(&id)
            .map_err(|e| ErrMode::Cut(Error::from(e)))?;

        let expr = if var.is_some() {
            let (expr,) = seq! {
                or_parse_error(Expr::parser, ParseErrorType::Expected("expression")),
            }
            .parse_next(input)?;
            expr
        } else {
            Expr::Literal(Literal::Nil)
        };
        input
            .state
            .define(&id)
            .map_err(|e| ErrMode::Cut(Error::from(e)))?;
        let depth = input.state.depth(&id);
        log::debug!("Declaring variable [{id}] with depth {depth:?}");
        let var = Statement::Var(id.into(), depth, expr);
        space_wrap(or_parse_error(";", ParseErrorType::Expected("';'")))
            .void()
            .parse_next(input)?;
        Ok(var)
    }

    fn expr_stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        terminated(
            Expr::parser.map(Statement::Expr),
            space_wrap(or_parse_error(";", ParseErrorType::Expected("';'"))),
        )
        .parse_next(input)
    }

    fn return_stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        trace("return statement", move |input: &mut Input<'s>| {
            let semicolon = |input: &mut Input<'s>| space_wrap(";").void().parse_next(input);
            full_word("return").parse_next(input)?;
            alt((
                terminated(Expr::parser, semicolon).map(Some),
                semicolon.map(|()| None),
            ))
            .map(|expr| Statement::Return(expr.unwrap_or(Expr::Literal(Literal::Nil))))
            .parse_next(input)
        })
        .parse_next(input)
    }

    fn print<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        trace("print statement", move |input: &mut Input<'s>| {
            let semicolon = |input: &mut Input<'s>| {
                space_wrap(or_parse_error(";", ParseErrorType::Expected(";")))
                    .void()
                    .parse_next(input)
            };
            full_word("print").parse_next(input)?;

            trace(
                "print expression",
                terminated(
                    or_parse_error(Expr::parser, ParseErrorType::Expected("expression")),
                    semicolon,
                )
                .map(Statement::Print),
            )
            .parse_next(input)
        })
        .parse_next(input)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub init: Option<Rc<Statement>>,
    pub condition: Expr,
    pub increment: Option<Expr>,
    pub body: Rc<Statement>,
}

impl std::fmt::Display for For {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "for ({init}; {cond}; {inc}) {body}",
            init = self
                .init
                .as_ref()
                .map_or(String::new(), |stmt| format!("{stmt}")),
            cond = self.condition,
            inc = self
                .increment
                .as_ref()
                .map_or(String::new(), |expr| format!("{expr}")),
            body = self.body,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Rc<str>,
    pub param_names: Rc<[Rc<str>]>,
    pub body: Rc<Statement>,
    pub initializer: bool,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fun {name}({params}) {body}",
            name = self.name,
            params = self.param_names.join(", "),
            body = self.body
        )
    }
}

pub fn parse(input: &str) -> Result<Ast, Error<Input<'_>>> {
    log::trace!("\n*** Begin parsing ***");
    let ast = Ast::parser.parse(Stateful::new(input, State::new(1)))?;
    log::trace!("*** Done parsing ***\n\n");
    Ok(ast)
}
