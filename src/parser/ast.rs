use winnow::{
    combinator::{alt, delimited, opt, preceded, repeat, terminated},
    seq, ModalResult, Parser,
};

use crate::{
    error::Error,
    interpreter::{Environment, EnvironmentView},
};

use super::{
    expr::{Expr, Literal},
    parse_error,
    state::State,
    tracking_multispace, Evaluate, Input, Run, Stateful,
};

#[derive(Debug)]
pub struct Ast {
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
    Var(String, Expr),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Option<Box<Statement>>, Expr, Option<Expr>, Box<Statement>),
}

impl Evaluate for Ast {
    fn evaluate<'s, 'env>(
        &'s self,
        env: &'env mut EnvironmentView,
    ) -> Result<Literal, Error<'s, Input<'s>>>
    where
        's: 'env,
    {
        let mut last = Literal::Nil;
        for statement in &self.statements {
            last = statement.evaluate(env)?;
        }

        Ok(last)
    }
}

impl Evaluate for Statement {
    fn evaluate<'s, 'env>(
        &'s self,
        env: &'env mut EnvironmentView,
    ) -> Result<Literal, Error<'s, Input<'s>>>
    where
        's: 'env,
    {
        match self {
            Statement::Expr(expr) => expr.evaluate(env),
            Statement::Print(expr) => {
                println!("{}", expr.evaluate(env)?);
                Ok(Literal::Nil)
            }
            Statement::Var(name, expr) => {
                let value = expr.evaluate(env)?;
                env.define(name, value.clone());
                log::debug!("define: {} = {:?}", name, value);
                log::debug!("environment: {:?}", env);
                Ok(value)
            }
            Statement::Block(statements) => {
                let mut last = Literal::Nil;
                let mut block_state = env.child_view();
                log::debug!("block environment: {:?}", block_state);
                for statement in statements {
                    last = statement.evaluate(&mut block_state)?;
                }
                Ok(last)
            }
            Statement::If(condition, true_s, false_s) => {
                let condition = bool::from(condition.evaluate(env)?);
                if condition {
                    true_s.evaluate(env)
                } else if let Some(false_s) = false_s {
                    false_s.evaluate(env)
                } else {
                    Ok(Literal::Nil)
                }
            }
            Statement::While(condition, stmt) => {
                let mut last = Literal::Nil;
                while bool::from(condition.evaluate(env)?) {
                    last = stmt.evaluate(env)?;
                }
                Ok(last)
            }
            Statement::For(init, condition, increment, body) => {
                let mut for_env = env.child_view();
                let mut last = Literal::Nil;
                if let Some(init) = init {
                    log::debug!("init: {:?}", init);
                    init.evaluate(&mut for_env)?;
                    log::debug!("init environment: {:?}", for_env);
                }
                while bool::from(condition.evaluate(&mut for_env)?) {
                    last = body.evaluate(&mut for_env)?;
                    log::debug!("after loop environment: {:?}", for_env);
                    if let Some(increment) = increment {
                        increment.evaluate(&mut for_env)?;
                    }
                    log::debug!("increment environment: {:?}", for_env);
                }
                Ok(last)
            }
        }
    }
}

impl Run for Ast {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>> {
        let mut env = Environment::new();
        let mut view = env.view();
        self.evaluate(&mut view)?;
        Ok(())
    }
}

impl Ast {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        repeat(0.., Statement::parser)
            .map(|statements| Self { statements })
            .parse_next(input)
    }
}

impl Statement {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        delimited(tracking_multispace, Self::declaration, tracking_multispace).parse_next(input)
    }

    fn declaration<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        alt((Self::var, Self::stmt)).parse_next(input)
    }

    fn stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        terminated(
            alt((
                Self::block,
                Self::condition,
                Self::while_loop,
                Self::for_loop,
                Self::print,
                Self::expr_stmt,
            )),
            tracking_multispace,
        )
        .parse_next(input)
    }

    fn for_loop<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        seq! {
            _: ("for", tracking_multispace, alt(("(", parse_error("Expect '('"))), tracking_multispace),
            alt((alt((Self::var , Self::expr_stmt)).map(Some), (tracking_multispace, ";", tracking_multispace).map(|_|None))),
            opt(Expr::parser),
            _: (tracking_multispace, ";", tracking_multispace),
            opt(Expr::parser),
            _: (tracking_multispace, alt((")", parse_error("Expect ')'"))), tracking_multispace),
            Self::stmt,
        }
        .map(|(init, condition, increment, body): (Option<Statement>, Option<Expr>, Option<Expr>, Statement)| {
            Statement::For(
                init.map(Box::new),
                condition.unwrap_or(Expr::Literal(Literal::from(true))),
                increment,
                Box::new(body),
            )
        })
        .parse_next(input)
    }

    fn while_loop<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        seq! {
            _: ("while", tracking_multispace, alt(("(", parse_error("Expect '('"))), tracking_multispace),
            Expr::parser,
            _: (tracking_multispace, alt((")", parse_error("Expect ')'"))), tracking_multispace),
            Self::stmt,
        }
        .map(|(condition, stmt)| Statement::While(condition, Box::new(stmt)))
        .parse_next(input)
    }

    fn condition<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        seq! {
            _: ("if", tracking_multispace, alt(("(", parse_error("Expect '('"))), tracking_multispace),
            Expr::parser,
            _: (tracking_multispace, alt((")", parse_error("Expect ')'"))), tracking_multispace),
            Self::stmt,
            opt(preceded((tracking_multispace, "else", tracking_multispace), Self::stmt)),
        }
        .map(|(condition, true_s, false_s)| Statement::If(condition, Box::new(true_s), false_s.map(Box::new)))
        .parse_next(input)
    }

    fn block<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        delimited(
            "{",
            Ast::parser.map(|ast| Statement::Block(ast.statements)),
            alt((("}", tracking_multispace), parse_error("Expect '}'"))),
        )
        .parse_next(input)
    }

    fn var<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        let (id, var) = seq! {
            _: terminated("var", tracking_multispace),
            alt((Literal::identifier, parse_error("expect identifier."))),
            opt((tracking_multispace, "=", tracking_multispace).void()),
        }
        .parse_next(input)?;

        let var = if var.is_some() {
            let (expr,) = seq! {
                alt((Expr::parser, parse_error("Expect expression"))),
            }
            .parse_next(input)?;
            Statement::Var(id, expr)
        } else {
            Statement::Var(id, Expr::Literal(Literal::Nil))
        };
        (
            tracking_multispace,
            alt((";", parse_error("Expect ';'"))),
            tracking_multispace,
        )
            .void()
            .parse_next(input)?;
        Ok(var)
    }

    fn expr_stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        terminated(
            Expr::parser.map(Statement::Expr),
            (
                tracking_multispace,
                alt((";", parse_error("Expect ';'"))),
                tracking_multispace,
            ),
        )
        .parse_next(input)
    }

    fn print<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        delimited(
            terminated("print", tracking_multispace),
            alt((Expr::parser, parse_error("expect expression."))),
            (
                tracking_multispace,
                alt((";", parse_error("Expect ';'"))),
                tracking_multispace,
            ),
        )
        .map(Statement::Print)
        .parse_next(input)
    }
}

pub fn parse(input: &str) -> Result<Ast, Error<'_, Input<'_>>> {
    let ast = Ast::parser.parse(Stateful::new(input, State::new(1)))?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mul_div() {
        let input = "1 / 2 * 3;";
        let ast = parse(input).unwrap();
        let res = ast.evaluate(&mut Environment::new().view());
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Literal::Number(1.0 / 2.0 * 3.0));
    }

    #[test]
    fn test_print_var() {
        let input = "var a = 1; print a;";
        let ast = parse(input).unwrap();
        let res = ast.run();
        assert!(res.is_ok());
    }

    #[test]
    fn test_print_empty() {
        let input = "print;";
        let ast = parse(input);
        assert!(ast.is_err());
        assert_eq!(
            ast.unwrap_err().to_string(),
            "[line 1] Error at ';': expect expression."
        );
    }

    #[test]
    fn test_var() {
        let input = r#"
// This program tests statements that don't have any side effects
19 - 93 >= -46 * 2 / 46 + 77;
true == true;
("hello" == "bar") == ("baz" != "world");
print true;
"#;
        let ast = parse(input).unwrap();
        let res = ast.run();
        assert!(res.is_ok());
    }

    #[test]
    fn test_block() {
        let input = r#"
            var a = 1;
            {
                var b = 2;
                a = 5;
                b = 3;
                a + b;
            }
        "#;
        let mut env = Environment::new();
        let mut view = env.view();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&mut view).unwrap();
        assert_eq!(res, Literal::Number(8.0));
    }

    #[test]
    fn test_condition() {
        let input = r#"if (true) {3;} else {4;}"#;
        let mut env = Environment::new();
        let mut view = env.view();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&mut view).unwrap();
        assert_eq!(res, Literal::Number(3.0));
    }

    #[test]
    fn test_for_loop() {
        let input = r#"
            var a = 0;
            for (var i = 0; i < 10; i = i + 1) {
                a = a + i;
            }
            a;
        "#;
        let mut env = Environment::new();
        let mut view = env.view();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&mut view).unwrap();
        assert_eq!(res, Literal::Number(45.0));
    }
}
