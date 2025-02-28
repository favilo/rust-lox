use std::fmt;

use winnow::{
    ascii::multispace0,
    combinator::{alt, eof, repeat, terminated},
    Parser,
};

/// Token represents a single token in the input.
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Eof => write!(f, "EOF  null"),
        }
    }
}

pub fn tokenize(input: &mut &str) -> winnow::error::Result<Vec<Token>> {
    terminated(
        repeat(
            0..,
            terminated(
                alt((
                    "(".map(|_| Token::LeftParen),
                    ")".map(|_| Token::RightParen),
                    "{".map(|_| Token::LeftBrace),
                    "}".map(|_| Token::RightBrace),
                )),
                multispace0,
            ),
        ),
        eof,
    )
    .map(|tokens: Vec<_>| tokens.into_iter().chain([Token::Eof]).collect::<Vec<_>>())
    .parse_next(input)
}
