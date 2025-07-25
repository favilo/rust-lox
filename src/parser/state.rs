use std::{
    collections::{HashMap, VecDeque},
    marker::PhantomData,
};

use winnow::{
    Parser,
    combinator::trace,
    error::{ErrMode, Needed},
    stream::{
        AsBStr, AsBytes, Compare, CompareResult, FindSlice, Location, Offset, SliceLen, Stream,
        StreamIsPartial, UpdateSlice,
    },
};

use crate::error::{Error, EvaluateError};

use super::Input;

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct Stateful<S> {
    input: S,
    pub(crate) state: State,
}

impl<S> Stateful<S> {
    pub fn new(input: S, state: State) -> Self {
        Self { input, state }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Copy, Hash)]
pub enum VarState {
    #[default]
    Declared,
    Defined,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct State {
    line: usize,
    scopes: VecDeque<HashMap<String, VarState>>,
}

impl State {
    pub fn new(line: usize) -> Self {
        let scopes = VecDeque::default();
        Self { line, scopes }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn inc_line(&mut self) {
        self.line += 1;
    }

    pub fn start_scope(&mut self) {
        self.scopes.push_back(HashMap::default());
    }

    pub fn end_scope(&mut self) {
        if !self.scopes.is_empty() {
            self.scopes.pop_back();
        }
    }

    pub fn scope_len(&self) -> usize {
        self.scopes.len()
    }

    pub fn scopes(&self) -> impl Iterator<Item = &HashMap<String, VarState>> {
        self.scopes.iter()
    }

    pub fn with_scope<'s, Output, P>(
        &'s mut self,
        mut f: P,
    ) -> impl Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>
    where
        P: Parser<Input<'s>, Output, ErrMode<Error<Input<'s>>>>,
    {
        trace("with_scope", move |input: &mut Input<'s>| {
            self.start_scope();
            let r = f.parse_next(input);
            self.end_scope();
            r
        })
    }

    pub fn declare(&mut self, name: &str) -> Result<(), EvaluateError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let scope = self
            .scopes
            .get_mut(self.scopes.len() - 1)
            .expect("Always at least one scope");
        if scope.contains_key(name) {
            return Err(EvaluateError::AlreadyDefined(name.to_string()));
        }
        scope.insert(name.to_string(), VarState::Declared);
        Ok(())
    }

    pub fn define(&mut self, name: &str) -> Result<(), EvaluateError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let scope = self
            .scopes
            .get_mut(self.scopes.len() - 1)
            .expect("Alwyas at least one scope");
        let Some(state) = scope.get_mut(name) else {
            return Err(EvaluateError::UndefinedVariable(name.to_string()));
        };
        if *state == VarState::Defined {
            return Err(EvaluateError::AlreadyDefined(name.to_string()));
        }
        *state = VarState::Defined;
        log::trace!("Define [{name}] in scope: {:?}", self.scopes);
        Ok(())
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name))
            .is_none_or(|scope| scope.get(name) == Some(&VarState::Defined))
    }

    pub fn is_declared(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name))
            .is_some_and(|scope| scope.get(name) == Some(&VarState::Declared))
    }

    pub fn depth(&mut self, name: &str) -> Option<usize> {
        self.scopes
            .iter()
            .rev()
            .enumerate()
            .find(|(_i, scope)| scope.contains_key(name))
            .filter(|(_, scope)| scope.get(name) == Some(&VarState::Defined))
            .map(|(i, _scope)| i)
    }
}

impl<I> AsRef<I> for Stateful<I> {
    fn as_ref(&self) -> &I {
        &self.input
    }
}

impl<I: std::fmt::Display> std::fmt::Display for Stateful<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.input.fmt(f)
    }
}

impl<I> SliceLen for Stateful<I>
where
    I: SliceLen,
{
    fn slice_len(&self) -> usize {
        self.input.slice_len()
    }
}

impl<I: Stream> Stream for Stateful<I> {
    type Token = <I as Stream>::Token;
    type Slice = <I as Stream>::Slice;

    type IterOffsets = <I as Stream>::IterOffsets;

    type Checkpoint = StateCheckpoint<I::Checkpoint, Self>;

    fn iter_offsets(&self) -> Self::IterOffsets {
        self.input.iter_offsets()
    }
    fn eof_offset(&self) -> usize {
        self.input.eof_offset()
    }

    fn next_token(&mut self) -> Option<Self::Token> {
        self.input.next_token()
    }

    fn peek_token(&self) -> Option<Self::Token> {
        self.input.peek_token()
    }

    fn offset_for<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Token) -> bool,
    {
        self.input.offset_for(predicate)
    }
    fn offset_at(&self, tokens: usize) -> Result<usize, Needed> {
        self.input.offset_at(tokens)
    }
    fn next_slice(&mut self, offset: usize) -> Self::Slice {
        self.input.next_slice(offset)
    }
    fn peek_slice(&self, offset: usize) -> Self::Slice {
        self.input.peek_slice(offset)
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        StateCheckpoint::<_, Self>::new(self.input.checkpoint(), self.state.clone())
    }
    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.input.reset(&checkpoint.inner);
        self.state = checkpoint.state.clone();
    }

    fn raw(&self) -> &dyn std::fmt::Debug {
        &self.input
    }
}

pub struct StateCheckpoint<C, S> {
    inner: C,
    state: State,
    stream: core::marker::PhantomData<S>,
}
impl<I, S> StateCheckpoint<I, S> {
    fn new(inner: I, state: State) -> Self {
        Self {
            inner,
            state,
            stream: PhantomData,
        }
    }
}

impl<T: Clone, S> Clone for StateCheckpoint<T, S> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            state: self.state.clone(),
            stream: PhantomData,
        }
    }
}

impl<T: PartialOrd, S> PartialOrd for StateCheckpoint<T, S> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl<T: Ord, S> Ord for StateCheckpoint<T, S> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: PartialEq, S> PartialEq for StateCheckpoint<T, S> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq, S> Eq for StateCheckpoint<T, S> {}

impl<T: std::fmt::Debug, S> std::fmt::Debug for StateCheckpoint<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<I, S> Offset for StateCheckpoint<I, S>
where
    I: Offset,
{
    fn offset_from(&self, start: &Self) -> usize {
        self.inner.offset_from(&start.inner)
    }
}

impl<I> Location for Stateful<I>
where
    I: Location,
{
    fn previous_token_end(&self) -> usize {
        self.input.previous_token_end()
    }
    fn current_token_start(&self) -> usize {
        self.input.current_token_start()
    }
}

impl<I> StreamIsPartial for Stateful<I>
where
    I: StreamIsPartial,
{
    type PartialState = I::PartialState;

    #[inline]
    fn complete(&mut self) -> Self::PartialState {
        self.input.complete()
    }

    #[inline]
    fn restore_partial(&mut self, state: Self::PartialState) {
        self.input.restore_partial(state);
    }

    fn is_partial_supported() -> bool {
        I::is_partial_supported()
    }

    fn is_partial(&self) -> bool {
        self.input.is_partial()
    }
}

impl<I> Offset for Stateful<I>
where
    I: Stream,
{
    fn offset_from(&self, start: &Self) -> usize {
        self.offset_from(&start.checkpoint())
    }
}

impl<I> Offset<<Stateful<I> as Stream>::Checkpoint> for Stateful<I>
where
    I: Stream,
{
    fn offset_from(&self, other: &<Stateful<I> as Stream>::Checkpoint) -> usize {
        self.checkpoint().offset_from(other)
    }
}

impl<I> AsBytes for Stateful<I>
where
    I: AsBytes,
{
    fn as_bytes(&self) -> &[u8] {
        self.input.as_bytes()
    }
}

impl<I> AsBStr for Stateful<I>
where
    I: AsBStr,
{
    fn as_bstr(&self) -> &[u8] {
        self.input.as_bstr()
    }
}

impl<I, U> Compare<U> for Stateful<I>
where
    I: Compare<U>,
{
    fn compare(&self, other: U) -> CompareResult {
        self.input.compare(other)
    }
}

impl<I, T> FindSlice<T> for Stateful<I>
where
    I: FindSlice<T>,
{
    fn find_slice(&self, substr: T) -> Option<std::ops::Range<usize>> {
        self.input.find_slice(substr)
    }
}

impl<I> UpdateSlice for Stateful<I>
where
    I: UpdateSlice,
{
    fn update_slice(mut self, inner: Self::Slice) -> Self {
        self.input = I::update_slice(self.input, inner);
        self
    }
}
