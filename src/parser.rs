use winnow::{
    combinator::{alt, delimited, eof, opt, repeat, repeat_till, trace},
    error::ErrMode,
    stream::Stream,
    token::{any, one_of},
    ModalResult, Parser,
};

use crate::{
    error::{Error, ParseError},
    interpreter::EnvironmentView,
    parser::{expr::Literal, state::Stateful},
};

pub mod ast;
pub mod expr;
pub mod state;

pub type InputStream<'s> = &'s str;
pub type Input<'s> = Stateful<InputStream<'s>>;

pub trait Evaluate {
    fn evaluate<'s, 'env>(
        &'s self,
        _: &'env mut EnvironmentView,
    ) -> Result<Literal, Error<'s, Input<'s>>>
    where
        's: 'env;
}

pub trait Run {
    fn run(&self) -> Result<(), Error<'_, Input<'_>>>;
}

pub fn comment<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<'s, Input<'s>>>
where
    for<'a> Input<'a>: Stream<Token = char>,
{
    trace(
        "comment",
        delimited(
            "//",
            repeat_till::<_, _, (), _, _, _, _>(0.., any, alt((tracking_new_line, eof.void()))),
            opt(tracking_new_line),
        ),
    )
    .void()
    .parse_next(input)
}

pub fn tracking_new_line<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<'s, Input<'s>>>
where
    for<'a> Input<'a>: Stream<Token = char>,
{
    trace(format!("tracking_new_line: {}", input.state.line()), '\n').parse_next(input)?;
    input.state.inc_line();
    Ok(())
}

pub fn whitespace<'s>(input: &mut Input<'s>) -> ModalResult<(), Error<'s, Input<'s>>> {
    trace(
        "tracking_multispace",
        repeat::<_, _, (), _, _>(
            0..,
            alt((one_of([' ', '\t', '\r']).void(), comment, tracking_new_line)),
        ),
    )
    .parse_next(input)
}

pub fn parse_error<'s, Output>(
    msg: &'static str,
) -> impl Parser<Input<'s>, Output, ErrMode<Error<'s, Input<'s>>>> {
    trace("parse_error", move |input: &mut Input<'s>| {
        whitespace.parse_next(input)?;
        Err(ErrMode::Cut(Error::Parse(ParseError::new(
            msg.to_string(),
            *input,
        ))))
    })
}
