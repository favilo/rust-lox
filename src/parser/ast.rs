use winnow::{
    combinator::{alt, delimited, preceded, repeat, terminated},
    LocatingSlice, ModalResult, Parser,
};

use crate::{error::Error, interpreter::InterpreterState};

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
}

impl Evaluate for Ast {
    fn evaluate(&self, state: &mut InterpreterState) -> Result<Literal, Error<'_, Input<'_>>> {
        let mut last = Literal::Nil;
        for statement in &self.statements {
            match statement {
                Statement::Expr(expr) => last = expr.evaluate(state)?,
                Statement::Print(expr) => {
                    last = Literal::Nil;
                    println!("{}", expr.evaluate(state)?)
                }
                Statement::Var(name, expr) => {
                    let value = expr.evaluate(state)?;
                    state.set(name, value.clone());
                    last = value;
                }
            }
        }

        Ok(last)
    }
}

impl Run for Ast {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>> {
        let mut state = InterpreterState::new();
        self.evaluate(&mut state)?;
        Ok(())
    }
}

impl Ast {
    fn parser<'s>(input: &mut Input<'s>) -> ModalResult<Self, Error<'s, Input<'s>>> {
        repeat(
            0..,
            delimited(
                tracking_multispace,
                alt((Self::print, Self::var, Expr::parser.map(Statement::Expr))),
                (
                    tracking_multispace,
                    alt((";", parse_error("Expect ';'"))),
                    tracking_multispace,
                ),
            ),
        )
        .map(|statements| Self { statements })
        .parse_next(input)
    }

    fn var<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        (
            delimited(
                terminated("var", tracking_multispace),
                alt((Literal::identifier, parse_error("expect identifier."))),
                alt((
                    (tracking_multispace, "=", tracking_multispace),
                    parse_error("expected '='"),
                )),
            ),
            Expr::parser,
        )
            .map(|(name, expr)| Statement::Var(name, expr))
            .parse_next(input)
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
        let res = ast.evaluate(&mut Default::default());
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
}
