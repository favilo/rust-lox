use std::{iter::once, sync::Arc};

use winnow::{
    ModalResult, Parser,
    combinator::{alt, opt, preceded, repeat, terminated, trace},
    error::ErrMode,
    seq,
};

use crate::{
    error::{Error, EvaluateError, ParseErrorType},
    interpreter::Context,
    parser::{full_word, or_parse_error, space_wrap},
};

use super::{
    Evaluate, Input, Run, Stateful, Value,
    expr::{Expr, Literal},
    parse_error,
    state::State,
};

#[cfg(test)]
mod tests;

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
    Var(String, Option<usize>, Expr),
    Block(Arc<[Statement]>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Option<Box<Statement>>, Expr, Option<Expr>, Box<Statement>),
    Function(String, Vec<String>, Arc<Statement>),
    Return(Expr),
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
            Statement::For(init, cond, inc, body) => write!(
                f,
                "for ({}; {cond}; {}) {body}",
                init.as_ref()
                    .map_or(String::new(), |stmt| format!("{stmt}")),
                inc.as_ref().map_or(String::new(), |expr| format!("{expr}")),
            ),
            Statement::Function(name, params, body) => {
                write!(f, "fun {name}({}) {body}", params.join(", "))
            }
            Statement::Return(expr) => write!(f, "return {expr};"),
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
            Statement::For(init, condition, increment, body) => {
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
            Statement::Return(expr) => {
                log::trace!("return: {}", expr);
                Err(EvaluateError::Return(expr.evaluate(env)?))
            }
            Statement::Function(name, params, body) => {
                log::trace!("function: {name}({}) {body}", params.join(", "));
                if !matches!(body.as_ref(), Statement::Block(_)) {
                    return Err(EvaluateError::FunctionBodyNotBlock(name.to_string()));
                }
                let value =
                    Value::Callable(name.to_string(), params.clone(), body.clone(), env.clone());
                let depth = env.depth();
                log::debug!("define: {name} = {value:#?} with depth {depth:?}");
                env.declare(name, value.clone(), depth)?;
                log::debug!("function definition global env: {env:?}");

                Ok(value)
            }
        }
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
        repeat(0.., Statement::parser)
            .map(|statements| Self { statements })
            .parse_next(input)
    }
}

impl Statement {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<Input<'s>>> {
        space_wrap(Self::declaration).parse_next(input)
    }

    fn declaration<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        alt((Self::function, Self::var_decl, Self::stmt)).parse_next(input)
    }

    fn function<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        full_word("fun").parse_next(input)?;

        let name = alt((
            Expr::identifier,
            parse_error(ParseErrorType::Expected("identifier")),
        ))
        .parse_next(input)?;
        input
            .state
            .declare(&name)
            .map_err(|e| ErrMode::Cut(Error::from(e)))?;

        input
            .state
            .define(&name)
            .map_err(|e| ErrMode::Cut(Error::from(e)))?;

        or_parse_error(space_wrap("("), ParseErrorType::Expected("'('")).parse_next(input)?;
        let params = Self::argument_names.parse_next(input)?;
        input.state.start_scope();
        log::debug!(
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
        or_parse_error(space_wrap(")"), ParseErrorType::Expected("')'")).parse_next(input)?;
        or_parse_error(space_wrap("{"), ParseErrorType::Expected("'{'")).parse_next(input)?;
        // Don't just use `Statement::block` because we don't want the extra scope
        let body = Ast::parser.parse_next(input)?;
        or_parse_error(space_wrap("}"), ParseErrorType::Expected("'}'")).parse_next(input)?;
        input.state.end_scope();
        log::debug!("Exiting function scope: {}", input.state.scope_len());
        Ok(Statement::Function(
            name,
            params,
            Statement::Block(body.statements.into()).into(),
        ))
    }

    fn argument_names<'s>(input: &mut Input<'s>) -> ModalResult<Vec<String>, Error<Input<'s>>> {
        let head = trace("first argument", opt(Expr::identifier)).parse_next(input)?;
        let Some(head) = head else {
            return Ok(vec![]);
        };

        let rest: Vec<String> = trace(
            "rest of arguments",
            repeat(0.., preceded(space_wrap(","), Expr::identifier)),
        )
        .parse_next(input)?;
        Ok(once(head).chain(rest).collect::<Vec<String>>())
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
        seq! {
            alt((alt((Self::var_decl , Self::expr_stmt)).map(Some), space_wrap(";").map(|_|None))),
            opt(Expr::parser),
            _: space_wrap(";"),
            opt(Expr::parser),
            _: space_wrap(alt((")", parse_error(ParseErrorType::Expected("')'"))))),
            Self::stmt,
        }
        .map(
            |(init, condition, increment, body): (
                Option<Statement>,
                Option<Expr>,
                Option<Expr>,
                Statement,
            )| {
                Statement::For(
                    init.map(Box::new),
                    condition.unwrap_or(Expr::Literal(Literal::from(true))),
                    increment,
                    Box::new(body),
                )
            },
        )
        .parse_next(input)
    }

    fn while_loop<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        (
            full_word("while"),
            space_wrap(or_parse_error("(", ParseErrorType::Expected("'('"))),
        )
            .parse_next(input)?;
        seq! {
            alt((Expr::parser, parse_error(ParseErrorType::Expected("expression")))),
            _: (space_wrap(or_parse_error(")", ParseErrorType::Expected("')'")))),
            alt((Self::stmt, parse_error(ParseErrorType::Expected("expression")))),
        }
        .map(|(condition, stmt)| Statement::While(condition, Box::new(stmt)))
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
            Self::stmt,
            opt(preceded(full_word("else"), Self::stmt)),
        }
        .map(|(condition, true_s, false_s)| {
            Statement::If(condition, Box::new(true_s), false_s.map(Box::new))
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
        let block = Ast::parser
            .map(|ast| Statement::Block(ast.statements.into()))
            .parse_next(input)?;
        log::debug!("Exiting block scope: {}", input.state.scope_len());
        input.state.end_scope();
        or_parse_error(space_wrap("}"), ParseErrorType::Expected("'}'")).parse_next(input)?;
        Ok(block)
    }

    fn var_decl<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        full_word("var").parse_next(input)?;
        let (id, var) = seq! {
            alt((Expr::identifier, parse_error(ParseErrorType::Expected("variable name")))),
            opt(space_wrap("=").void()),
        }
        .parse_next(input)?;
        input
            .state
            .declare(&id)
            .map_err(|e| ErrMode::Cut(Error::from(e)))?;

        let expr = if var.is_some() {
            let (expr,) = seq! {
                alt((Expr::parser, parse_error(ParseErrorType::Expected("expression")))),
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
        let var = Statement::Var(id, depth, expr);
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
        let semicolon = |input: &mut Input<'s>| space_wrap(";").void().parse_next(input);
        full_word("return").parse_next(input)?;
        alt((
            terminated(Expr::parser, semicolon).map(Some),
            semicolon.map(|()| None),
        ))
        .map(|expr| Statement::Return(expr.unwrap_or(Expr::Literal(Literal::Nil))))
        .parse_next(input)
    }

    fn print<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<Input<'s>>> {
        let semicolon = |input: &mut Input<'s>| {
            space_wrap(or_parse_error(";", ParseErrorType::Expected(";")))
                .void()
                .parse_next(input)
        };
        full_word("print").parse_next(input)?;

        terminated(
            or_parse_error(Expr::parser, ParseErrorType::Expected("expression")),
            semicolon,
        )
        .map(Statement::Print)
        .parse_next(input)
    }
}

pub fn parse(input: &str) -> Result<Ast, Error<Input<'_>>> {
    log::trace!("\n*** Begin parsing ***");
    let ast = Ast::parser.parse(Stateful::new(input, State::new(1)))?;
    log::trace!("*** Done parsing ***\n\n");
    Ok(ast)
}
