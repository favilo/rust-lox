use std::hash::Hash;
use winnow::{
    ascii::{digit1, multispace0, Caseless},
    combinator::{alt, cut_err, delimited, empty, fail, opt, preceded, repeat, terminated, trace},
    dispatch,
    error::ParserError,
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    token::{any, one_of, take_till},
    LocatingSlice, ModalResult, Parser,
};

use crate::error;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Group(Box<Expr>),
    Unary(Unary),
    Binary(Binary),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{}", l),
            Self::Unary(u) => write!(f, "{}", u),
            Self::Binary(b) => write!(f, "{}", b),
            Self::Group(e) => write!(f, "(group {})", e),
        }
    }
}

impl Expr {
    pub fn parser<S, E>(input: &mut S) -> ModalResult<Self, E>
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
        trace(
            "expr",
            delimited(
                multispace0,
                alt((
                    // Binary::parser.map(Self::Binary),
                    Expr::equality,
                )),
                multispace0,
            ),
        )
        .parse_next(input)
    }

    pub fn equality<S, E>(input: &mut S) -> ModalResult<Expr, E>
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
        let init = trace("First comparison", Expr::comparison).parse_next(input)?;

        trace(
            "rest of equality",
            repeat(0.., (alt(("==", "!=")), Expr::comparison)).fold(
                move || init.clone(),
                |acc, (op, val): (S::Slice, Expr)| match op.as_bstr() {
                    b"==" => Expr::Binary(Binary::Equals(Box::new(acc), Box::new(val))),
                    b"!=" => Expr::Binary(Binary::NotEquals(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn comparison<S, E>(input: &mut S) -> ModalResult<Expr, E>
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
        let init = trace("First term", Expr::term).parse_next(input)?;

        trace(
            "rest of comparison",
            repeat(0.., (alt(("<=", ">=", "<", ">")), Expr::term)).fold(
                move || init.clone(),
                |acc, (op, val): (S::Slice, Expr)| match op.as_bstr() {
                    b"<=" => Expr::Binary(Binary::LessEq(Box::new(acc), Box::new(val))),
                    b">=" => Expr::Binary(Binary::GreaterEq(Box::new(acc), Box::new(val))),
                    b"<" => Expr::Binary(Binary::LessThan(Box::new(acc), Box::new(val))),
                    b">" => Expr::Binary(Binary::GreaterThan(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn term<S, E>(input: &mut S) -> ModalResult<Expr, E>
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
        let init = trace("First factor", Expr::factor).parse_next(input)?;

        trace(
            "rest of term",
            repeat(0.., (one_of(['+', '-']), Expr::factor)).fold(
                move || init.clone(),
                |acc, (op, val): (S::Token, Expr)| match op.as_char() {
                    '+' => Expr::Binary(Binary::Add(Box::new(acc), Box::new(val))),
                    '-' => Expr::Binary(Binary::Sub(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn factor<S, E>(input: &mut S) -> ModalResult<Expr, E>
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
        let init = trace("First unary", Expr::unary).parse_next(input)?;

        trace(
            "rest of factor",
            repeat(0.., (one_of(['*', '/']), Expr::unary)).fold(
                move || init.clone(),
                |acc, (op, val): (S::Token, Expr)| match op.as_char() {
                    '*' => Expr::Binary(Binary::Mul(Box::new(acc), Box::new(val))),
                    '/' => Expr::Binary(Binary::Div(Box::new(acc), Box::new(val))),
                    _ => unreachable!(),
                },
            ),
        )
        .parse_next(input)
    }

    pub fn unary<S, E>(input: &mut S) -> ModalResult<Expr, E>
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
                (
                    delimited(multispace0, operator, multispace0),
                    terminated(Expr::unary, multispace0),
                )
                    .map(|(neg, e)| match neg {
                        Sign::Negate => Expr::Unary(Unary::Negate(Box::new(e))),
                        Sign::Not => Expr::Unary(Unary::Not(Box::new(e))),
                    }),
            ),
            trace("no unary", Expr::primary),
        ))
        .parse_next(input)
    }

    pub fn primary<S, E>(input: &mut S) -> ModalResult<Self, E>
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
        trace(
            "primary",
            delimited(
                multispace0,
                alt((Expr::parenthesis, Literal::literal.map(Expr::Literal))),
                multispace0,
            ),
        )
        .parse_next(input)
    }

    fn parenthesis<S, E>(input: &mut S) -> ModalResult<Self, E>
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
        trace(
            "parenthesis",
            preceded(
                "(",
                cut_err(delimited(multispace0, Self::parser, (multispace0, ")"))),
            ),
        )
        .map(|e| Self::Group(Box::new(e)))
        .parse_next(input)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unary {
    Negate(Box<Expr>),
    Not(Box<Expr>),
}

impl std::fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Negate(e) => write!(f, "(- {})", e),
            Self::Not(e) => write!(f, "(! {})", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binary {
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    NotEquals(Box<Expr>, Box<Expr>),
}

impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mul(l, r) => write!(f, "(* {} {})", l, r),
            Self::Div(l, r) => write!(f, "(/ {} {})", l, r),
            Self::Add(l, r) => write!(f, "(+ {} {})", l, r),
            Self::Sub(l, r) => write!(f, "(- {} {})", l, r),
            Self::LessThan(l, r) => write!(f, "(< {} {})", l, r),
            Self::GreaterThan(l, r) => write!(f, "(> {} {})", l, r),
            Self::LessEq(l, r) => write!(f, "(<= {} {})", l, r),
            Self::GreaterEq(l, r) => write!(f, "(>= {} {})", l, r),
            Self::Equals(l, r) => write!(f, "(== {} {})", l, r),
            Self::NotEquals(l, r) => write!(f, "(!= {} {})", l, r),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    True,
    False,
    Nil,
    Number(f64),
    String(String),
}

impl Literal {
    pub fn literal<S, E>(input: &mut S) -> ModalResult<Self, E>
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
        trace(
            "literal",
            delimited(
                multispace0,
                alt((
                    "true".value(Self::True),
                    "false".value(Self::False),
                    "nil".value(Self::Nil),
                    Self::number,
                    Self::string,
                )),
                multispace0,
            ),
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

    fn string<S, E>(input: &mut S) -> ModalResult<Self, E>
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
        trace(
            "string",
            preceded(
                "\"",
                cut_err(terminated(
                    take_till(0.., '"'),
                    "\"",
                    // .context(StrContext::Expected("terminating `\"`".into())),
                )),
            ),
        )
        .map(|s: S::Slice| Self::String(std::str::from_utf8(s.as_bstr()).unwrap().to_string()))
        .parse_next(input)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
            Self::Number(n) => write!(f, "{n:?}"),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

pub fn parse(input: &str) -> Result<Expr, error::Error> {
    let expr = Expr::parser::<_, winnow::error::TreeError<_>>
        .parse(LocatingSlice::new(input))
        .map_err(|e| error::Error::ParseError(format!("{e}")))?;

    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_bool() -> anyhow::Result<()> {
        let input = "true\n";
        let res = parse(input)?;
        assert_eq!(res, Expr::Literal(Literal::True));

        let input = "false   \t";
        let res = parse(input)?;
        assert_eq!(res, Expr::Literal(Literal::False));
        Ok(())
    }

    #[test]
    fn test_nil() -> anyhow::Result<()> {
        let input = "     nil   ";
        let res = parse(input)?;
        assert_eq!(res, Expr::Literal(Literal::Nil));
        Ok(())
    }

    #[test]
    fn test_number() -> anyhow::Result<()> {
        let input = "123.45";
        let res = parse(input)?;
        assert_eq!(res, Expr::Literal(Literal::Number(123.45)));
        Ok(())
    }

    #[test]
    fn test_string() -> anyhow::Result<()> {
        let input = "\n \"hello, world!\"";
        let res = parse(input)?;
        assert_eq!(
            res,
            Expr::Literal(Literal::String("hello, world!".to_string()))
        );
        Ok(())
    }

    #[test]
    fn test_unary() -> anyhow::Result<()> {
        let input = "-123.45    ";
        let res = parse(input)?;
        assert_eq!(
            res,
            Expr::Unary(Unary::Negate(Box::new(Expr::Literal(Literal::Number(
                123.45
            )))))
        );

        Ok(())
    }

    #[test]
    fn test_paren() -> anyhow::Result<()> {
        let input = "  !(  -123.45  )\t\n";
        let res = parse(input)?;
        assert_eq!(
            res,
            Expr::Unary(Unary::Not(Box::new(Expr::Group(Box::new(Expr::Unary(
                Unary::Negate(Box::new(Expr::Literal(Literal::Number(123.45))))
            ))))))
        );

        Ok(())
    }

    #[test]
    fn test_term() -> anyhow::Result<()> {
        let input = "123.45 * 67.89 / 10.11";
        let res = parse(input)?;
        assert_eq!(
            res,
            Expr::Binary(Binary::Div(
                Box::new(Expr::Binary(Binary::Mul(
                    Box::new(Expr::Literal(Literal::Number(123.45))),
                    Box::new(Expr::Literal(Literal::Number(67.89)))
                ))),
                Box::new(Expr::Literal(Literal::Number(10.11)))
            ))
        );

        Ok(())
    }

    #[test]
    fn test_term_codecrafters() -> anyhow::Result<()> {
        let input = "(76 * -50 / (56 * 42))";
        let res = parse(input)?;
        assert_eq!(
            res.to_string(),
            "(group (/ (* 76.0 (- 50.0)) (group (* 56.0 42.0))))",
        );

        Ok(())
    }

    #[test]
    fn test_add_sub_codecrafters() -> anyhow::Result<()> {
        let input = "(72 * -62 / (42 * 98))";
        let res = parse(input)?;
        assert_eq!(
            res.to_string(),
            "(group (/ (* 72.0 (- 62.0)) (group (* 42.0 98.0))))",
        );
        Ok(())
    }

    #[test]
    fn test_hello_plus_world() -> anyhow::Result<()> {
        let input = r#""hello" + "world""#;
        let res = parse(input)?;
        assert_eq!(res.to_string(), "(+ hello world)",);
        Ok(())
    }

    #[test]
    fn test_comparison() -> anyhow::Result<()> {
        let input = "83 < 99 < 115";
        let res = parse(input)?;
        assert_eq!(res.to_string(), "(< (< 83.0 99.0) 115.0)");
        Ok(())
    }
}
