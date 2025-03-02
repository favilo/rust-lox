use std::hash::Hash;
use winnow::{
    ascii::{multispace0, Caseless},
    combinator::{alt, delimited},
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    ModalResult, Parser,
};

use crate::error;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
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
        alt((
            Binary::parser.map(Self::Binary),
            Literal::parser.map(Self::Literal),
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
            )),
            multispace0,
        )
        .parse_next(input)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

pub fn parse(input: &str) -> Result<(), error::Error> {
    let expr = Literal::parser
        .parse(input)
        .map_err(|e| error::Error::ParseError(format!("{e}")))?;

    println!("{}", expr);

    Ok(())
}
