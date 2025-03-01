use std::{fmt, hash::Hash, str::FromStr};

use winnow::{
    ascii::{float, line_ending, space0, Caseless},
    combinator::{alt, delimited, eof, opt, preceded, repeat_till},
    error::{ErrMode, Result, StrContext},
    seq,
    stream::{AsBStr, AsChar, Compare, ParseSlice, Stream, StreamIsPartial},
    token::{any, take_till, take_while},
    LocatingSlice, ModalResult, Parser,
};

use crate::error::Error;

/// Token represents a single token in the input.
#[derive(Debug, Clone, PartialEq)]
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
    Number(String, f64),
    Identifier(String),

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

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

    fn number<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<Caseless<&'a str>> + AsBStr + Compare<char>,
        S::Slice: Eq + Hash + AsBStr + ParseSlice<f64>,
        S::Token: AsChar + Clone,
        S::IterOffsets: Clone,
    {
        float
            .take()
            .map(|s: S::Slice| {
                let s = std::str::from_utf8(s.as_bstr()).unwrap().to_string();
                let number = f64::from_str(&s).unwrap();
                Self::Number(s, number)
            })
            .parse_next(input)
    }

    fn identifier<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<&'a str> + AsBStr,
        S::Slice: Eq + Hash + AsBStr,
        S::Token: AsChar + Clone,
    {
        (
            any.verify(|c: &S::Token| {
                let c = c.clone().as_char();
                c.is_alphabetic() || c == '_'
            }),
            take_while(0.., |c: S::Token| {
                let c = c.as_char();
                c.is_alphanumeric() || c == '_'
            }),
        )
            .take()
            .map(|s: S::Slice| {
                Self::Identifier(std::str::from_utf8(s.as_bstr()).unwrap().to_string())
            })
            .parse_next(input)
    }

    fn keyword<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream + StreamIsPartial + Compare<&'a str> + AsBStr,
        S::Slice: Eq + Hash + AsBStr,
        S::Token: AsChar + Clone,
    {
        alt((
            "and".value(Token::And),
            "class".value(Token::Class),
            "else".value(Token::Else),
            "false".value(Token::False),
            "for".value(Token::For),
            "fun".value(Token::Fun),
            "if".value(Token::If),
            "nil".value(Token::Nil),
            "or".value(Token::Or),
            "print".value(Token::Print),
            "return".value(Token::Return),
            "super".value(Token::Super),
            "this".value(Token::This),
            "true".value(Token::True),
            "var".value(Token::Var),
            "while".value(Token::While),
        ))
        .parse_next(input)
    }

    fn parser<S>(input: &mut S) -> ModalResult<Self>
    where
        for<'a> S: Stream
            + StreamIsPartial
            + Compare<&'a str>
            + Compare<Caseless<&'a str>>
            + AsBStr
            + Compare<char>,
        S::Slice: Eq + Hash + AsBStr + ParseSlice<f64>,
        S::Token: AsChar + Clone,
        S::IterOffsets: Clone,
    {
        delimited(
            space0,
            alt((
                Self::comment,
                Self::string,
                Self::keyword,
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
                Self::number,
                Self::identifier,
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
            Token::Number(s, n) => write!(f, "NUMBER {s} {n:?}"),
            Token::Identifier(s) => write!(f, "IDENTIFIER {s} null"),

            Token::And => write!(f, "AND and null"),
            Token::Class => write!(f, "CLASS class null"),
            Token::Else => write!(f, "ELSE else null"),
            Token::False => write!(f, "FALSE false null"),
            Token::For => write!(f, "FOR for null"),
            Token::Fun => write!(f, "FUN fun null"),
            Token::If => write!(f, "IF if null"),
            Token::Nil => write!(f, "NIL nil null"),
            Token::Or => write!(f, "OR or null"),
            Token::Print => write!(f, "PRINT print null"),
            Token::Return => write!(f, "RETURN return null"),
            Token::Super => write!(f, "SUPER super null"),
            Token::This => write!(f, "THIS this null"),
            Token::True => write!(f, "TRUE true null"),
            Token::Var => write!(f, "VAR var null"),
            Token::While => write!(f, "WHILE while null"),

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
