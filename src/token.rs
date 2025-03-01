use std::{fmt, hash::Hash};

use winnow::{
    ascii::{line_ending, space0},
    combinator::{alt, cut_err, delimited, eof, opt, preceded, repeat_till, terminated},
    error::{ErrMode, Result, StrContext},
    seq,
    stream::{AsBStr, AsChar, Compare, Stream, StreamIsPartial},
    token::{any, take_till},
    LocatingSlice, ModalResult, Parser,
};

use crate::error::Error;

/// Token represents a single token in the input.
#[derive(Debug, Clone, PartialEq, Eq)]
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

    Equal,
    EqualEqual,

    Bang,
    BangEqual,

    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    StringLiteral(String),

    Newline,
    Unexpected(usize, String),
    Unterminated(usize),

    Eof,
}

impl Token {
    fn comment<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<&'a str>,
        S::Slice: Eq + Hash + AsBStr,
        S::Token: AsChar + Clone,
    {
        seq!(
            "//",
            repeat_till::<_, _, (), _, _, _, _>(0.., any, alt((line_ending, eof))),
            opt(line_ending)
        )
        .value(Self::Newline)
        .parse_next(input)
    }

    fn string<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<&'a str>,
        S::Slice: Eq + Hash + AsBStr,
        S::Token: AsChar + Clone,
    {
        preceded("\"", (take_till(0.., '"'), opt("\"")))
            .map(|(s, exits): (S::Slice, Option<_>)| {
                if exits.is_none() {
                    Self::Unterminated(0)
                } else {
                    Self::StringLiteral(std::str::from_utf8(s.as_bstr()).unwrap().to_string())
                }
            })
            .context(StrContext::Expected("string".into()))
            .parse_next(input)
    }

    fn parser<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<&'a str>,
        S::Slice: Eq + Hash + AsBStr,
        S::Token: AsChar + Clone,
    {
        delimited(
            space0,
            alt((
                Self::comment,
                Self::string,
                // Single character tokens
                alt((
                    "(".value(Token::LeftParen),
                    ")".value(Token::RightParen),
                    "{".value(Token::LeftBrace),
                    "}".value(Token::RightBrace),
                    ".".value(Token::Dot),
                    ",".value(Token::Comma),
                    "-".value(Token::Minus),
                    "+".value(Token::Plus),
                    "*".value(Token::Star),
                    "/".value(Token::Slash),
                    ";".value(Token::Semicolon),
                )),
                alt((
                    // Must come before later substring tokens
                    "==".value(Token::EqualEqual),
                    "!=".value(Token::BangEqual),
                    "<=".value(Token::LessEqual),
                    ">=".value(Token::GreaterEqual),
                    // Must come after the multi-char tokens
                    "=".value(Token::Equal),
                    "!".value(Token::Bang),
                    "<".value(Token::Less),
                    ">".value(Token::Greater),
                )),
                line_ending.value(Token::Newline),
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

            Token::Equal => write!(f, "EQUAL = null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::Less => write!(f, "LESS < null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),

            Token::StringLiteral(s) => write!(f, "STRING \"{s}\" {s}"),

            Token::Newline => Ok(()),
            Token::Eof => write!(f, "EOF  null"),
            Token::Unexpected(line, s) => {
                write!(f, "[line {line}] Error: Unexpected character: {s}")
            }
            Token::Unterminated(line) => write!(f, "[line {line}] Error: Unterminated string."),
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
            Ok(Token::Unterminated(_)) => {
                errors = true;
                Some(Token::Unterminated(line))
            }
            Ok(token) => Some(token),
            Err(ErrMode::Cut(e)) => {
                eprintln!("Cut: {e:?}");
                None
            }
            Err(ErrMode::Incomplete(e)) => {
                eprintln!("Incomplete: {e:?}");
                None
            }
            Err(ErrMode::Backtrack(_)) => {
                let slice = input.next_token()?;
                errors = true;
                Some(Token::Unexpected(line, slice.to_string()))
            }
        }
    });

    for token in &mut iter {
        match token {
            Token::Newline => {}
            Token::Unexpected(_, _) | Token::Unterminated(_) => eprintln!("{}", token),
            Token::Eof => break,
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
