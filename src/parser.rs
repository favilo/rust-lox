use std::hash::Hash;
use winnow::{
    ascii::{float, multispace0, Caseless},
    combinator::{alt, cut_err, delimited, opt, preceded, terminated},
    error::StrContext,
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    token::take_till,
    ModalResult, Parser,
};

use crate::error;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Parenthesis(Box<Expr>),
    Unary(Unary),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{}", l),
            Self::Unary(u) => write!(f, "{}", u),
            Self::Parenthesis(e) => write!(f, "(group {})", e),
        }
    }
}

impl Expr {
    pub fn parser<S>(input: &mut S) -> ModalResult<Self>
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
    {
        delimited(
            multispace0,
            alt((
                // Binary::parser.map(Self::Binary),
                Self::parenthesis,
                Unary::parser.map(Self::Unary),
                Literal::parser.map(Self::Literal),
            )),
            multispace0,
        )
        .parse_next(input)
    }

    fn parenthesis<S>(input: &mut S) -> ModalResult<Self>
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
    {
        preceded(
            "(",
            cut_err(delimited(multispace0, Self::parser, (multispace0, ")"))),
        )
        .map(|e| Self::Parenthesis(Box::new(e)))
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

impl Unary {
    pub fn parser<S>(input: &mut S) -> ModalResult<Self>
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
    {
        alt((
            preceded("-", Expr::parser.map(Box::new).map(Self::Negate)),
            preceded("!", Expr::parser.map(Box::new).map(Self::Not)),
        ))
        .parse_next(input)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binary {
    Add(Box<Expr>, Box<Expr>),
}

impl Binary {
    pub fn parser<S>(input: &mut S) -> ModalResult<Self>
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
    {
        let left = Expr::parser.parse_next(input)?;
        let _ = "+".parse_next(input)?;
        let right = Expr::parser.parse_next(input)?;
        Ok(Self::Add(Box::new(left), Box::new(right)))
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
    pub fn parser<S>(input: &mut S) -> ModalResult<Self>
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
    {
        delimited(
            multispace0,
            alt((
                "true".value(Self::True),
                "false".value(Self::False),
                "nil".value(Self::Nil),
                float.map(Self::Number),
                Self::string,
            )),
            multispace0,
        )
        .parse_next(input)
    }

    fn string<S>(input: &mut S) -> ModalResult<Self>
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
    {
        preceded(
            "\"",
            cut_err(terminated(
                take_till(0.., '"'),
                "\"".context(StrContext::Expected("terminating `\"`".into())),
            )),
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

pub fn parse(input: &str) -> Result<(), error::Error> {
    let expr = Expr::parser
        .parse(input)
        .map_err(|e| error::Error::ParseError(format!("{e}")))?;

    println!("{}", expr);

    Ok(())
}
