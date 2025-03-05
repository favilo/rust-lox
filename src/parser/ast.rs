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
                Ok(value)
            }
            Statement::Block(statements) => {
                let mut last = Literal::Nil;
                let mut block_state = env.child_view();
                for statement in statements {
                    last = statement.evaluate(&mut block_state)?;
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
        repeat(
            0..,
            preceded(
                tracking_multispace,
                alt((
                    Self::block,
                    terminated(
                        alt((Self::print, Self::var, Expr::parser.map(Statement::Expr))),
                        (
                            tracking_multispace,
                            alt((";", parse_error("Expect ';'"))),
                            tracking_multispace,
                        ),
                    ),
                )),
            ),
        )
        .map(|statements| Self { statements })
        .parse_next(input)
    }

    fn block<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        delimited(
            "{",
            Self::parser.map(|ast| Statement::Block(ast.statements)),
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

        if var.is_some() {
            let expr = alt((Expr::parser, parse_error("Expect expression"))).parse_next(input)?;
            Ok(Statement::Var(id, expr))
        } else {
            Ok(Statement::Var(id, Expr::Literal(Literal::Nil)))
        }
    }

    fn print<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        preceded(
            terminated("print", tracking_multispace),
            alt((Expr::parser, parse_error("expect expression."))),
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
}
