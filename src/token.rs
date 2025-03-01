use std::fmt;

use winnow::{
    ascii::{line_ending, space0},
    combinator::{alt, delimited},
    error::{ErrMode, Result},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    LocatingSlice, ModalResult, Parser,
};

use crate::error::Error;

/// Token represents a single token in the input.
#[derive(Debug)]
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

    Newline,
    Unexpected(usize, String),

    Eof,
}

impl Token {
    fn parser<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<&'a str>,
        S::Token: AsChar + Clone,
    {
        delimited(
            space0,
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
                line_ending.map(|_| Token::Newline),
            )),
            space0,
        )
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

            Token::Newline => Ok(()),
            Token::Eof => write!(f, "EOF  null"),
            Token::Unexpected(offset, s) => {
                write!(f, "[line {offset}] Error: Unexpected character: {s}")
            }
        }
    }
}

pub fn tokenize(input: &str) -> Result<(), crate::error::Error> {
    let mut input = LocatingSlice::new(input);
    let mut line = 1;
    let mut errors = false;
    let mut iter = std::iter::from_fn(|| {
        if input.is_empty() {
            return None;
        }
        match Token::parser.parse_next(&mut input) {
            Ok(Token::Newline) => {
                line += 1;
                Some(Token::Newline)
            }
            Ok(token) => Some(token),
            Err(ErrMode::Cut(e)) => {
                eprintln!("Cut: {e}");
                None
            }
            Err(ErrMode::Incomplete(e)) => {
                eprintln!("Incomplete: {e:?}");
                None
            }
            Err(ErrMode::Backtrack(_)) => {
                errors = true;
                let slice = input.next_token()?;
                Some(Token::Unexpected(line, slice.to_string()))
            }
        }
    });

    for token in &mut iter {
        match token {
            Token::Newline => {}
            Token::Unexpected(_, _) => eprintln!("{}", token),
            _ => println!("{}", token),
        }
    }
    println!("{}", Token::Eof);

    if errors {
        Err(Error::TokenizeError("Tokenization failed".into()))
    } else {
        Ok(())
    }
}
