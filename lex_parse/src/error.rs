use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
};
use std::marker::PhantomData;

use nom::{error::{ContextError, ErrorKind as NomErrorKind, FromExternalError, ParseError}, InputLength};
pub use nom_supreme::error::ErrorTree;
use nom_supreme::tag::TagError;

#[derive(Debug)]
pub struct ErrorIgnorerT<I> {
    phantom: PhantomData<I>,
}

impl<I> ErrorIgnorerT<I> {}

impl<I: Display> Display for ErrorIgnorerT<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl<I: Display + Debug> Error for ErrorIgnorerT<I> {}

impl<I: InputLength> ParseError<I> for ErrorIgnorerT<I> {
    fn from_error_kind(_: I, _: NomErrorKind) -> Self {
        ErrorIgnorerT { phantom: PhantomData }
    }

    fn append(_: I, _: NomErrorKind, _: Self) -> Self {
        ErrorIgnorerT { phantom: PhantomData }
    }

    fn from_char(_: I, _: char) -> Self {
        ErrorIgnorerT { phantom: PhantomData }
    }

    fn or(self, _: Self) -> Self {
        ErrorIgnorerT { phantom: PhantomData }
    }
}

impl<I> ContextError<I> for ErrorIgnorerT<I> {
    fn add_context(_: I, _: &'static str, other: Self) -> Self {
        other
    }
}

impl<I, E: Error + Send + Sync + 'static> FromExternalError<I, E> for ErrorIgnorerT<I> {
    fn from_external_error(_: I, _kind: NomErrorKind, _: E) -> Self {
        ErrorIgnorerT { phantom: PhantomData }
    }
}

impl<I> TagError<I, &'static str> for ErrorIgnorerT<I> {
    fn from_tag(_: I, _: &'static str) -> Self {
        ErrorIgnorerT { phantom: PhantomData }
    }
}

pub trait ET<'a>: ParseError<&'a [u8]> + ContextError<&'a [u8]> + TagError<&'a [u8], &'static str> {}

impl<'a, E> ET<'a> for E where E: ParseError<&'a [u8]> + ContextError<&'a [u8]> + TagError<&'a [u8], &'static str> {}

pub type ErrorIgnorer<'a> = ErrorIgnorerT<&'a [u8]>;
