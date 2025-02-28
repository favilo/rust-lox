use std::fmt;

use winnow::{
    ascii::multispace0,
    combinator::{alt, eof, repeat, terminated},
    error::Result,
    Parser,
};

/// Token represents a single token in the input.
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Dot,
    Comma,
    Minus,
    Plus,
    Star,
    Slash,
    Semicolon,
    Eof,
}

impl Token {
    fn parser(input: &mut &str) -> Result<Self> {
        alt((
            "(".map(|_| Token::LeftParen),
            ")".map(|_| Token::RightParen),
            "{".map(|_| Token::LeftBrace),
            "}".map(|_| Token::RightBrace),
            ".".map(|_| Token::Dot),
            ",".map(|_| Token::Comma),
            "-".map(|_| Token::Minus),
            "+".map(|_| Token::Plus),
            "*".map(|_| Token::Star),
            "/".map(|_| Token::Slash),
            ";".map(|_| Token::Semicolon),
        ))
        .parse_next(input)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::Star => write!(f, "STAR * null"),
            Token::Slash => write!(f, "SLASH / null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),

            Token::Eof => write!(f, "EOF  null"),
        }
    }
}

pub fn tokenize(input: &mut &str) -> Result<Vec<Token>> {
    terminated(repeat(0.., terminated(Token::parser, multispace0)), eof)
        .map(|tokens: Vec<_>| tokens.into_iter().chain([Token::Eof]).collect::<Vec<_>>())
        .parse_next(input)
}
