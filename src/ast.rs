use winnow::{
    combinator::{alt, delimited, repeat, terminated},
    LocatingSlice, ModalResult, Parser,
};

use crate::{
    error::Error,
    parser::{
        parse_error,
        state::{State, Stateful},
        tracking_multispace, Expr, Input, Literal,
    },
};

#[derive(Debug)]
pub struct Ast {
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
}

pub trait Evaluate {
    fn evaluate(&self) -> Result<Literal, Error<'_, Input<'_>>>;
}

pub trait Run {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>>;
}

impl Evaluate for Ast {
    fn evaluate(&self) -> Result<Literal, Error<'_, Input<'_>>> {
        let mut last = Literal::Nil;
        for statement in &self.statements {
            match statement {
                Statement::Expr(expr) => last = expr.evaluate()?,
                Statement::Print(expr) => {
                    last = Literal::Nil;
                    println!("{}", expr.evaluate()?)
                }
            }
        }

        Ok(last)
    }
}

impl Run for Ast {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>> {
        self.evaluate()?;
        Ok(())
    }
}

impl Ast {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        repeat(
            0..,
            delimited(
                tracking_multispace,
                alt((
                    (delimited(
                        terminated("print", tracking_multispace),
                        Expr::parser,
                        alt((";", parse_error("Expect ';'"))),
                    )
                    .map(Statement::Print)),
                    terminated(Expr::parser.map(Statement::Expr), ";"),
                )),
                tracking_multispace,
            ),
        )
        .map(|statements| Self { statements })
        .parse_next(input)
    }
}

pub fn parse(input: &str) -> Result<Ast, Error<'_, Input<'_>>> {
    let ast = Ast::parser.parse(Stateful::new(LocatingSlice::new(input), State::new(1)))?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mul_div() {
        let input = "1 / 2 * 3;";
        let ast = parse(input).unwrap();
        let res = ast.evaluate();
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Literal::Number(1.0 / 2.0 * 3.0));
    }
}
