use crate::{
    error::Error,
    parser::{Expr, Input, Literal},
};

pub struct Ast {
    expr: Expr,
}

impl Ast {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }

    pub fn evaluate(&self) -> Result<Literal, Error<'_, Input<'_>>> {
        self.expr.evaluate()
    }
}
