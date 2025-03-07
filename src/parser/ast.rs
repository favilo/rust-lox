use std::iter::once;

use winnow::{
    ascii::alpha1,
    combinator::{alt, delimited, opt, preceded, repeat, terminated, trace},
    seq,
    stream::Stream,
    ModalResult, Parser,
};

use crate::{
    error::{Error, EvaluateError, ParseErrorType},
    interpreter::Context,
    parser::whitespace1,
};

use super::{
    expr::{Expr, Literal},
    parse_error,
    state::State,
    whitespace, Evaluate, Input, Run, Stateful, Value,
};

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
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(Option<Box<Statement>>, Expr, Option<Expr>, Box<Statement>),
    Function(String, Vec<String>, Box<Statement>),
    Return(Expr),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => write!(f, "{expr};"),
            Statement::Print(expr) => write!(f, "print {expr};"),
            Statement::Var(_, _depth, expr) => write!(f, "var = {expr};"),
            Statement::Block(vec) => {
                writeln!(f, "{{")?;
                for stmt in vec {
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
                env.declare(name, value.clone(), *depth);
                log::debug!("define: {} = {:?}", name, value);
                log::debug!("environment: {:?}", env);
                Ok(value)
            }
            Statement::Block(statements) => {
                log::trace!("block: {:?}", statements);
                let mut last = Value::Nil;
                let block_env = env.child();
                log::debug!("block environment: {:?}", block_env);
                for statement in statements {
                    let result = statement.evaluate(&block_env);
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
                let value = Value::Callable(
                    name.to_string(),
                    params.clone(),
                    body.clone().into(),
                    env.clone(),
                );
                let depth = env.depth();
                log::debug!("define: {name} = {value:#?} with depth {depth:?}");
                env.declare(name, value.clone(), depth);
                log::debug!("function definition global env: {env:?}");

                Ok(value)
            }
        }
    }
}

impl Run for Ast {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>> {
        let env = Context::new();
        log::trace!("*** Begin running ***");
        self.evaluate(&env)?;
        log::trace!("*** Done running ***\n");
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
        delimited(whitespace, Self::declaration, whitespace).parse_next(input)
    }

    fn declaration<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        alt((Self::function, Self::var_decl, Self::stmt)).parse_next(input)
    }

    fn function<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        (
            alpha1.verify(|id: <Input as Stream>::Slice| id == "fun"),
            whitespace,
        )
            .parse_next(input)?;

        let name = alt((
            Expr::identifier,
            parse_error(ParseErrorType::Expected("identifier")),
        ))
        .parse_next(input)?;
        input.state.define(&name);
        alt((
            (whitespace, "(", whitespace),
            parse_error(ParseErrorType::Expected("'('")),
        ))
        .parse_next(input)?;
        let params = Self::argument_names.parse_next(input)?;
        input.state.start_scope();
        log::debug!(
            "Entering function params scope: {:?}",
            input.state.scopes().collect::<Vec<_>>()
        );
        params.iter().for_each(|param| input.state.define(param));
        alt((
            (whitespace, ")", whitespace),
            parse_error(ParseErrorType::Expected("')'")),
        ))
        .parse_next(input)?;
        let body = Self::block.parse_next(input)?;
        input.state.end_scope();
        log::debug!("Exiting function scope: {}", input.state.scope_len());
        Ok(Statement::Function(name, params, Box::new(body)))
    }

    fn argument_names<'s>(input: &mut Input<'s>) -> ModalResult<Vec<String>, Error<'s, Input<'s>>> {
        let head = trace("first argument", opt(Expr::identifier)).parse_next(input)?;
        let Some(head) = head else {
            return Ok(vec![]);
        };

        let rest: Vec<String> = trace(
            "rest of arguments",
            repeat(
                0..,
                preceded((whitespace, ",", whitespace), Expr::identifier),
            ),
        )
        .parse_next(input)?;
        Ok(once(head).chain(rest).collect::<Vec<String>>())
    }

    fn stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        terminated(
            alt((
                Self::block,
                Self::condition,
                Self::while_loop,
                Self::for_loop,
                Self::print,
                Self::return_stmt,
                Self::expr_stmt,
            )),
            whitespace,
        )
        .parse_next(input)
    }

    fn for_loop<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        seq! {
            _: ("for", whitespace, alt(("(", parse_error(ParseErrorType::Expected("'('")))), whitespace),
            alt((alt((Self::var_decl , Self::expr_stmt)).map(Some), (whitespace, ";", whitespace).map(|_|None))),
            opt(Expr::parser),
            _: (whitespace, ";", whitespace),
            opt(Expr::parser),
            _: (whitespace, alt((")", parse_error(ParseErrorType::Expected("')'")))), whitespace),
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
            _: ("while", whitespace, alt(("(", parse_error(ParseErrorType::Expected("'('")))), whitespace),
            Expr::parser,
            _: (whitespace, alt((")", parse_error(ParseErrorType::Expected("')'")))), whitespace),
            Self::stmt,
        }
        .map(|(condition, stmt)| Statement::While(condition, Box::new(stmt)))
        .parse_next(input)
    }

    fn condition<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        seq! {
            _: ("if", whitespace, alt(("(", parse_error(ParseErrorType::Expected("'('")))), whitespace),
            Expr::parser,
            _: (whitespace, alt((")", parse_error(ParseErrorType::Expected("')'")))), whitespace),
            Self::stmt,
            opt(preceded((whitespace, "else", whitespace), Self::stmt)),
        }
        .map(|(condition, true_s, false_s)| {
            Statement::If(condition, Box::new(true_s), false_s.map(Box::new))
        })
        .parse_next(input)
    }

    fn block<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        "{".parse_next(input)?;
        input.state.start_scope();
        log::debug!(
            "Entering block scope: {:?}",
            input.state.scopes().collect::<Vec<_>>()
        );
        let block = Ast::parser
            .map(|ast| Statement::Block(ast.statements))
            .parse_next(input)?;
        log::debug!("Exiting block scope: {}", input.state.scope_len());
        input.state.end_scope();
        alt((
            ("}", whitespace),
            parse_error(ParseErrorType::Expected("'}'")),
        ))
        .parse_next(input)?;
        Ok(block)
    }

    fn var_decl<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        terminated(
            alpha1.verify(|id: <Input as Stream>::Slice| id == "var"),
            whitespace,
        )
        .parse_next(input)?;
        let (id, var) = seq! {
            alt((Expr::identifier, parse_error(ParseErrorType::Expected("identifier")))),
            opt((whitespace, "=", whitespace).void()),
        }
        .parse_next(input)?;
        input.state.declare(&id);

        let expr = if var.is_some() {
            let (expr,) = seq! {
                alt((Expr::parser, parse_error(ParseErrorType::Expected("expression")))),
            }
            .parse_next(input)?;
            expr
        } else {
            Expr::Literal(Literal::Nil)
        };
        input.state.define(&id);
        let depth = input.state.depth(&id);
        log::debug!("Declaring variable [{id}] with depth {depth:?}");
        let var = Statement::Var(id, depth, expr);
        (
            whitespace,
            alt((";", parse_error(ParseErrorType::Expected("';'")))),
            whitespace,
        )
            .void()
            .parse_next(input)?;
        Ok(var)
    }

    fn expr_stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        terminated(
            Expr::parser.map(Statement::Expr),
            (
                whitespace,
                alt((";", parse_error(ParseErrorType::Expected("';'")))),
                whitespace,
            ),
        )
        .parse_next(input)
    }

    fn return_stmt<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        let semicolon =
            |input: &mut Input<'s>| (whitespace, ";", whitespace).void().parse_next(input);
        seq! {
            _: alpha1.verify(|id: <Input as Stream>::Slice| id == "return"),
            alt((delimited(whitespace1, Expr::parser, semicolon).map(Some), semicolon.map(|()| None))),
        }
        .map(|(expr,)| Statement::Return(expr.unwrap_or(Expr::Literal(Literal::Nil))))
        .parse_next(input)
    }

    fn print<'s>(input: &mut Input<'s>) -> ModalResult<Statement, Error<'s, Input<'s>>> {
        let semicolon = |input: &mut Input<'s>| {
            (
                whitespace,
                alt((";", parse_error(ParseErrorType::Expected("expression")))),
                whitespace,
            )
                .void()
                .parse_next(input)
        };
        seq! {
            _: (alpha1.verify(|id: <Input as Stream>::Slice| id == "print"), whitespace),
            terminated(alt((Expr::parser, parse_error(ParseErrorType::Expected("expression")))), semicolon),
        }
        .map(|(expr,)| Statement::Print(expr))
        .parse_next(input)
    }
}

pub fn parse(input: &str) -> Result<Ast, Error<'_, Input<'_>>> {
    log::trace!("\n*** Begin parsing ***");
    let ast = Ast::parser.parse(Stateful::new(input, State::new(1)))?;
    log::trace!("*** Done parsing ***\n\n");
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_utils::test_std_lox_test;

    #[test]
    fn test_mul_div() {
        let input = "1 / 2 * 3;";
        let ast = parse(input).unwrap();
        let res = ast.evaluate(&Context::new());
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Value::Number(1.0 / 2.0 * 3.0));
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
    fn test_print_not_keyword() {
        let input = "
            fun printAndModify() {}
            printAndModify();";
        let ast = parse(input).unwrap();
        let res = ast.run();
        assert!(res.is_ok());
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
        let input = r"
            var a = 1;
            {
                var b = 2;
                a = 5;
                b = 3;
                a + b;
            }
        ";
        let env = Context::new();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&env).unwrap();
        assert_eq!(res, Value::Number(8.0));
    }

    #[test]
    fn test_condition() {
        let input = r"if (true) {3;} else {4;}";
        let env = Context::new();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&env).unwrap();
        assert_eq!(res, Value::Number(3.0));
    }

    #[test]
    fn test_for_loop() {
        let input = r"
            var a = 0;
            for (var i = 0; i < 10; i = i + 1) {
                a = a + i;
            }
            a;
        ";
        let env = Context::new();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&env).unwrap();
        assert_eq!(res, Value::Number(45.0));
    }

    #[test]
    fn test_nested_fn() {
        let input = r"
            fun fib(n) {
              if (n < 2) return n;
              return fib(n - 2) + fib(n - 1);
            }
            fib(10);
        ";

        let env = Context::new();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&env).unwrap();
        assert_eq!(res, Value::Number(55.0));
    }

    #[test]
    fn test_static_scope() {
        let input = r#"
            var a = "global";
            {
              fun showA() {
                print a;
              }

              showA();
              var a = "block";
              showA();
            }"#;

        let env = Context::new();
        let ast: Ast = parse(input).unwrap();
        let res = ast.evaluate(&env).unwrap();
        assert_eq!(res, Value::Nil);
    }

    mod closure {
        use super::*;

        // Classes required
        // test_std_lox_test!(close_over_method_parameter, closure, Value::Nil, "a\n");

        test_std_lox_test!(unused_later_closure, closure, Value::Nil, "a\n");
        test_std_lox_test!(unused_closure, closure, Value::Nil, "ok\n");
        test_std_lox_test!(reference_closure_multiple_times, closure, Value::Nil, "a\na\n");
        test_std_lox_test!(open_closure_in_function, closure, Value::Nil, "local\n");
        test_std_lox_test!(closed_closure_in_function, closure, Value::Nil, "local\n");
        test_std_lox_test!(close_over_later_variable, closure, Value::Nil, "b\na\n");
        test_std_lox_test!(
            close_over_function_parameter,
            closure,
            Value::Nil,
            "param\n"
        );
        test_std_lox_test!(
            assign_to_shadowed_later,
            closure,
            Value::Nil,
            "inner\nassigned\n"
        );
        test_std_lox_test!(
            assign_to_closure,
            closure,
            Value::Nil,
            "local\nafter f\nafter f\nafter g\n"
        );
        test_std_lox_test!(nested_closure, closure, Value::Nil, "a\nb\nc\n");
        test_std_lox_test!(
            shadow_closure_with_local,
            closure,
            Value::Nil,
            "closure\nshadow\nclosure\n"
        );
        test_std_lox_test!(reuse_closure_slot, closure, Value::Nil, "a\n");
    }
}
