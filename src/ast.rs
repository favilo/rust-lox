use crate::{
    error::Error,
    parser::{Expr, Input, Literal},
};

pub struct Ast {
    expr: Expr,
}

pub trait Evaluate {
    fn evaluate(&self) -> Result<Literal, Error<'_, Input<'_>>>;
}

impl Ast {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

impl Evaluate for Ast {
    fn evaluate(&self) -> Result<Literal, Error<'_, Input<'_>>> {
        self.expr.evaluate()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_mul_div() {
        let input = "1 / 2 * 3";
        let expr = parse(input).unwrap();
        let ast = Ast::new(expr);
        let res = ast.evaluate();
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), Literal::Number(1.0 / 2.0 * 3.0));
    }
}
