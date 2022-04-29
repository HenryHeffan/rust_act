use super::basic::{ast::Ctrl, ctrl};
use crate::ast::{FTPtr, Kw};
use core::marker::PhantomData;
use nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{cut, not, peek, recognize},
    error::{ContextError, ParseError},
    And, IResult, InputLength, Offset, Parser, Slice,
};
use nom_supreme::{error::ErrorTree, parser_ext::*};
use std::fmt::Debug;
use std::{ops::RangeTo, str};

pub type ET<'a> = ErrorTree<&'a [u8]>;

#[derive(Debug, Clone)]
pub struct SepList1<T, U> {
    pub items: Vec<T>,
    pub seps: Vec<U>,
}

// First the clones of the nom_supreme parsers that allow accessing the seperator as well as the main parser item

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ZeroLengthParseState<E> {
    None,
    Item,
    Separator { terminator_error: E },
}
impl<E> ZeroLengthParseState<E> {
    fn terminator_error(self) -> Option<E> {
        match self {
            Self::Separator { terminator_error } => Some(terminator_error),
            _ => None,
        }
    }
}
/// Shared implementation for parse_separated_terminated_res and
/// parse_separated_terminated. This exists so that we don't have to have an
/// unnecessary bound of FromExternalError on parse_separated_terminated.
#[inline]
fn parse_separated_terminated_impl<Input, ParseOutput, SepOutput, TermOutput, ParseErr, Accum, FoldErr>(
    mut parser: impl Parser<Input, ParseOutput, ParseErr>,
    mut separator: impl Parser<Input, SepOutput, ParseErr>,
    mut terminator: impl Parser<Input, TermOutput, ParseErr>,

    mut init: impl FnMut(ParseOutput) -> Accum,
    mut fold: impl FnMut(Accum, SepOutput, ParseOutput) -> Result<Accum, FoldErr>,

    mut build_error: impl FnMut(Input, FoldErr) -> ParseErr,
) -> impl Parser<Input, (Accum, TermOutput), ParseErr>
where
    Input: Clone + InputLength,
    ParseErr: ParseError<Input>,
{
    use nom::{error::ErrorKind::SeparatedNonEmptyList, Err::Error};
    move |mut input: Input| {
        let mut sep: Option<SepOutput> = None;
        let mut accum = None;

        let mut zero_length_state = ZeroLengthParseState::None;

        loop {
            // Try to find a value. To fail to do so at this point is an
            // error, since we either just started or successfully parsed a
            // separator.
            //
            // If an error occurs here, also try to attach terminator_error.
            // terminator_error is available if the most recent separator parse
            // was zero-length, which means that both the terminator and the
            // item would be valid parses at this point.
            let (tail, value) = match parser.parse(input.clone()) {
                Ok(success) => success,
                Err(Error(item_error)) => {
                    break Err(Error(match zero_length_state.terminator_error() {
                        None => item_error,
                        Some(terminator_error) => item_error.or(terminator_error),
                    }))
                }
                Err(err) => break Err(err),
            };

            // Check zero-length matches
            zero_length_state = match (input.input_len() == tail.input_len(), zero_length_state) {
                // If both the item and the separator had a zero length match,
                // we're hanging. Bail.
                //
                // It doesn't make sense to include the terminator error here,
                // because we *did* successfully parse a separator and an
                // item, they just happened to be zero length
                (true, ZeroLengthParseState::Separator { .. }) => {
                    break Err(Error(ParseErr::from_error_kind(input, SeparatedNonEmptyList)))
                }

                // If only the item had a zero-length match, update the
                // state.
                (true, _) => ZeroLengthParseState::Item,

                // If the item had a non-zero length match, clear the state
                (false, _) => ZeroLengthParseState::None,
            };

            // Advance the loop state
            if sep.is_some() {
                accum = Some(fold(accum.unwrap(), sep.unwrap(), value).map_err(|err| Error(build_error(input, err)))?);
            } else {
                accum = Some(init(value));
            }
            input = tail;

            // Try to find a terminator; if we found it, we're done. If we
            // didn't, preserve the error, so that it can be reported as an
            // .or() branch with the subsequent separator or item error.
            let terminator_error = match terminator.parse(input.clone()) {
                // We found a terminator, so we're done
                Ok((tail, term_out)) => break Ok((tail, (accum.unwrap(), term_out))),

                // No terminator. Keep track of the error in case we also fail
                // to find a separator or item.
                Err(Error(err)) => err,

                // Other kinds of errors should be returned immediately.
                Err(err) => break Err(err),
            };

            // No terminator, so instead try to find a separator
            let (tail, tail_sep) = match separator.parse(input.clone()) {
                Ok((tail, tail_sep)) => (tail, tail_sep),
                Err(Error(separator_error)) => break Err(Error(separator_error.or(terminator_error))),
                Err(err) => break Err(err),
            };

            // Check zero-length matches
            zero_length_state = match (input.input_len() == tail.input_len(), zero_length_state) {
                // If both the separator and the item had a zero length match,
                // we're hanging. Bail.
                (true, ZeroLengthParseState::Item) => {
                    break Err(Error(ParseErr::from_error_kind(input, SeparatedNonEmptyList)))
                }
                // If only the separator had a zero-length match, update the
                // state. Additionally preserve the terminator error so that
                // it can be reported as an alternate if there was an item
                // error.
                (true, _) => ZeroLengthParseState::Separator { terminator_error },

                // If the separator had a non-zero length match, clear the
                // state
                (false, _) => ZeroLengthParseState::None,
            };

            // Advance the loop state
            input = tail;
            sep = Some(tail_sep);
        }
    }
}
use core::convert::Infallible;

#[inline]
fn parse_separated_terminated<Input, ParseOutput, SepOutput, TermOutput, ParseErr, Accum>(
    parser: impl Parser<Input, ParseOutput, ParseErr>,
    separator: impl Parser<Input, SepOutput, ParseErr>,
    terminator: impl Parser<Input, TermOutput, ParseErr>,

    init: impl FnMut(ParseOutput) -> Accum,
    mut fold: impl FnMut(Accum, SepOutput, ParseOutput) -> Accum,
) -> impl Parser<Input, (Accum, TermOutput), ParseErr>
where
    Input: Clone + InputLength,
    ParseErr: ParseError<Input>,
{
    parse_separated_terminated_impl(
        parser,
        separator,
        terminator,
        init,
        move |accum, sep, item| Ok(fold(accum, sep, item)),
        |_input, err: Infallible| match err {},
    )
}

#[inline]
fn collect_separated_terminated<Input, ParseOutput, SepOutput, TermOutput, ParseErr>(
    parser: impl Parser<Input, ParseOutput, ParseErr>,
    separator: impl Parser<Input, SepOutput, ParseErr>,
    terminator: impl Parser<Input, TermOutput, ParseErr>,
) -> impl Parser<Input, (SepList1<ParseOutput, SepOutput>, TermOutput), ParseErr>
where
    Input: Clone + InputLength,
    ParseErr: ParseError<Input>,
{
    parse_separated_terminated(
        parser,
        separator,
        terminator,
        |item| SepList1 {
            items: vec![item],
            seps: Vec::new(),
        },
        |mut collection, sep, item| {
            collection.items.push(item);
            collection.seps.push(sep);
            collection
        },
    )
}

// Then the helper classes for parsing sequences of character

#[derive(Copy, Clone, Debug)]
pub enum CtrlN {
    Char1,
    Char2,
    Char3,
    Char1NotChar2,
}

#[derive(Debug, Clone, Copy)]
pub struct CtrlC {
    pub v: CtrlN,
    pub spaced: &'static [u8; 3],
    pub unspaced: &'static [u8; 3],
    pub label: &'static str,
}

impl<'a> Parser<&'a [u8], Ctrl, ET<'a>> for CtrlC {
    #[inline]
    fn parse(&mut self, input: &'a [u8]) -> IResult<&'a [u8], Ctrl, ET<'a>> {
        match self.v {
            CtrlN::Char1 => tag(&self.spaced[0..1])
                .or(tag(&self.unspaced[0..1]))
                .map(|vs: &[u8]| Ctrl::Ctrl(FTPtr::of_ptr(&vs[0])))
                .context(self.label)
                .parse(input),
            CtrlN::Char2 => tag(&self.spaced[0..2])
                .or(tag(&self.unspaced[0..2]))
                .map(|vs: &[u8]| Ctrl::Ctrl2(FTPtr::of_ptr(&vs[0]), FTPtr::of_ptr(&vs[1])))
                .context(self.label)
                .parse(input),
            CtrlN::Char3 => tag(&self.spaced[0..3])
                .or(tag(&self.unspaced[0..3]))
                .map(|vs: &[u8]| Ctrl::Ctrl3(FTPtr::of_ptr(&vs[0]), FTPtr::of_ptr(&vs[1]), FTPtr::of_ptr(&vs[2])))
                .context(self.label)
                .parse(input),
            CtrlN::Char1NotChar2 => peek(not(tag(&self.spaced[0..2]).or(tag(&self.unspaced[0..2]))))
                .ignore_then(tag(&self.spaced[0..1]).or(tag(&self.unspaced[0..1])))
                .map(|vs: &[u8]| Ctrl::Ctrl(FTPtr::of_ptr(&vs[0])))
                .context(self.label)
                .parse(input),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct KwC {
    pub buf: &'static [u8; 1],
    pub label: &'static str,
}

impl<'a> Parser<&'a [u8], Kw, ET<'a>> for KwC {
    #[inline]
    fn parse(&mut self, input: &'a [u8]) -> IResult<&'a [u8], Kw, ET<'a>> {
        tag(&self.buf[0..1])
            .map(|vs: &[u8]| Kw(FTPtr::of_ptr(&vs[0])))
            .context(self.label)
            .parse(input)
    }
}

// The types below help simplify the code in ast.rs for handling terminated lists. They delay
// the instatiation of list1_sep_by calls until a terminator is provided (or explicitly skiped
// with `.p()`)

/// Parser which gets and discards a delimiting value both before and after the
/// main subparser. Returns the output from the main subparser if all were
/// successful.
#[derive(Debug, Clone, Copy)]
pub struct MyDelim<P, L, OL, R, OR> {
    parser: P,
    l: L,
    r: R,
    _phantom1: PhantomData<OL>,
    _phantom2: PhantomData<OR>,
}

impl<'a, I, P, OP, L, OL, R, OR, E> Parser<I, (OL, OP, OR), E> for MyDelim<P, L, OL, R, OR>
where
    P: Parser<I, OP, E>,
    L: Parser<I, OL, E> + Copy + Clone + Sized,
    R: Parser<I, OR, E> + Copy + Clone + Sized,
    E: ParseError<I> + ContextError<I>,
{
    #[inline]
    fn parse(&mut self, input: I) -> IResult<I, (OL, OP, OR), E> {
        let (input, l) = self.l.parse(input)?;
        let (input, value) = self.parser.parse(input)?;
        let (input, r) = self.r.parse(input)?;
        Ok((input, (l, value, r)))
    }
}

pub trait ParserExt2<I, O, E>: ParserExt<I, O, E> + Sized {
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn then_ignore<F, O2>(self, terminator: F) -> Terminated<Self, F, O2>
    where
        F: Parser<I, O2, E>,
    {
        self.terminated(terminator)
    }

    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn ignore_then<F, O2>(self, successor: F) -> Preceded<F, Self, O>
    where
        F: Parser<I, O2, E>,
    {
        self.precedes(successor)
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn ignore_then_opt<F, O2>(self, successor: F) -> Preceded<Optional<F>, Self, O>
    where
        F: Parser<I, O2, E>,
        I: std::clone::Clone,
    {
        self.ignore_then(successor.opt())
    }

    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn then<F, O2>(self, f: F) -> And<Self, F>
    where
        F: Parser<I, O2, E>,
    {
        self.and(f)
    }

    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn then_opt<F, O2>(self, f: F) -> And<Self, Optional<F>>
    where
        F: Parser<I, O2, E>,
        I: Clone,
    {
        self.then(f.opt())
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn then_cut<F, O2>(self, f: F) -> And<Self, Cut<F>>
    where
        F: Parser<I, O2, E>,
        I: Clone,
    {
        self.then(f.cut())
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn ignore_then_cut<F, O2>(self, f: F) -> Preceded<Cut<F>, Self, O>
    where
        F: Parser<I, O2, E>,
        I: Clone,
    {
        self.ignore_then(f.cut())
    }
    fn delim_by<L, R, OL, OR>(self, l: L, r: R) -> MyDelim<Self, L, OL, R, OR>
    where
        L: Parser<I, OL, E> + Copy + Clone + Sized,
        R: Parser<I, OR, E> + Copy + Clone + Sized,
    {
        MyDelim {
            parser: self,
            l,
            r,
            _phantom1: PhantomData,
            _phantom2: PhantomData,
        }
    }
}
impl<I, O, E, P> ParserExt2<I, O, E> for P where P: Parser<I, O, E> {}

pub trait MyParserExt<'a, O>: Parser<&'a [u8], O, ET<'a>> + Sized {
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn list1_sep_by(self, sep: CtrlC) -> Unterm<Self> {
        Unterm::make(self, sep)
    }
    fn many0(self) -> Many0<Self> {
        Many0::make(self)
    }
    fn many1(self) -> Many1<Self> {
        Many1::make(self)
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn parened(self) -> MyDelim<Self, CtrlC, Ctrl, CtrlC, Ctrl> {
        self.delim_by(ctrl('('), ctrl(')'))
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn braced(self) -> MyDelim<Self, CtrlC, Ctrl, CtrlC, Ctrl> {
        self.delim_by(ctrl('{'), ctrl('}'))
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn bracketed(self) -> MyDelim<Self, CtrlC, Ctrl, CtrlC, Ctrl> {
        self.delim_by(ctrl('['), ctrl(']'))
    }
    #[inline]
    #[must_use = "Parsers do nothing unless used"]
    fn ang_braced(self) -> MyDelim<Self, CtrlC, Ctrl, CtrlC, Ctrl> {
        self.delim_by(ctrl('<'), ctrl('>'))
    }
}
impl<'a, O, P: Parser<&'a [u8], O, ET<'a>>> MyParserExt<'a, O> for P {}

// these classes allow for simple syntax when writing parsers while allowing for good error handling in list1_sep_by

pub struct UntermT<F, Sep> {
    f: F,
    sep: Sep,
}
pub struct OptUntermT<F, Sep> {
    ut: UntermT<F, Sep>,
}

pub type Unterm<T> = UntermT<T, CtrlC>;

pub struct Many1<F> {
    f: F,
}
pub struct Many0<F> {
    f: F,
}

impl<F, Sep> UntermT<F, Sep> {
    #[inline]
    pub fn make<I, OF, OS, E>(f: F, sep: Sep) -> Self
    where
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        UntermT { f, sep }
    }

    #[inline]
    pub fn p<I, OF, OS, E>(self) -> impl Parser<I, SepList1<OF, OS>, E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        collect_separated_terminated(self.f, self.sep, peek(not(self.sep))).map(|(a, _)| a)
    }

    #[inline]
    pub fn terminated<I, OF, OS, G, OG, E>(self, term: G) -> impl Parser<I, (SepList1<OF, OS>, OG), E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        collect_separated_terminated(self.f, self.sep, term)
    }

    #[inline]
    pub fn opt<I, OF, OS, E>(self) -> OptUntermT<F, Sep>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        OptUntermT::make(self)
    }
}

impl<F, Sep> OptUntermT<F, Sep> {
    #[inline]
    pub fn make<I, OF, OS, E>(ut: UntermT<F, Sep>) -> Self
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        OptUntermT { ut }
    }

    #[inline]
    pub fn terminated<I, OF, OS, G, OG, E>(self, term: G) -> impl Parser<I, (Option<SepList1<OF, OS>>, OG), E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone + Sized,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        alt((
            self.ut.term_by(term).map(|(v, t)| (Some(v), t)),
            term.map(|t| (None, t)),
        ))
    }
}

impl<F> Many1<F> {
    #[inline]
    pub fn make<I, OF, E>(f: F) -> Self
    where
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
    {
        Many1 { f }
    }

    #[inline]
    pub fn terminated<I, OF, G, OG, E>(self, terminator: G) -> impl Parser<I, (Vec<OF>, OG), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E>,
    {
        parse_separated_terminated(
            self.f,
            take(0usize),
            terminator,
            |e| vec![e],
            move |mut l, _, e| {
                l.push(e);
                l
            },
        )
    }

    #[inline]
    pub fn term_by_peek_alt2<I, OF, G, OG, H, OH, E>(self, term1: G, term2: H) -> impl Parser<I, Vec<OF>, E>
    where
        I: nom::InputIter + nom::InputTake + Offset + Slice<RangeTo<usize>> + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E>,
        H: Parser<I, OH, E>,
    {
        self.terminated(peek(recognize(term1).or(recognize(term2))))
            .map(|(a, _)| a)
    }
}

impl<F> Many0<F> {
    #[inline]
    pub fn make<I, OF, E>(f: F) -> Self
    where
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
    {
        Many0 { f }
    }

    #[inline]
    fn terminated_x2<I, OF, G, OG, E>(self, terminator1: G, terminator2: G) -> impl Parser<I, (Vec<OF>, OG), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E>,
    {
        alt((
            terminator1.map(|t| (Vec::new(), t)),
            cut(parse_separated_terminated(
                self.f,
                take(0usize),
                terminator2,
                |e| vec![e],
                move |mut l, _, e| {
                    l.push(e);
                    l
                },
            )),
        ))
    }

    #[inline]
    fn terminated<I, OF, G, OG, E>(self, terminator: G) -> impl Parser<I, (Vec<OF>, OG), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone,
    {
        self.terminated_x2(terminator, terminator)
    }
    #[inline]
    pub fn terminated_fn<I, OF, G, OG, H, E>(self, terminator: H) -> impl Parser<I, (Vec<OF>, OG), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        H: Fn() -> G,
        G: Parser<I, OG, E>,
    {
        self.terminated_x2(terminator(), terminator())
    }

    #[inline]
    pub fn term_by_peek_not<I, OF, G, OG, E>(self, not_term: G) -> impl Parser<I, Vec<OF>, E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone,
    {
        self.terminated_x2(peek(not(not_term)), peek(not(not_term)))
            .map(|(a, _)| a)
    }

    #[inline]
    pub fn term_by_peek_not_alt2<I, OF, G, OG, H, OH, E>(self, not_term1: G, not_term2: H) -> impl Parser<I, Vec<OF>, E>
    where
        I: nom::InputIter + nom::InputTake + Offset + Slice<RangeTo<usize>> + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone,
        H: Parser<I, OH, E> + Copy + Clone,
    {
        self.terminated_x2(
            peek(not(recognize(not_term1).or(recognize(not_term2)))),
            peek(not(recognize(not_term1).or(recognize(not_term2)))),
        )
        .map(|(a, _)| a)
    }

    #[inline]
    pub fn term_by_peek_alt2<I, OF, G, OG, H, OH, E>(self, not_term1: G, not_term2: H) -> impl Parser<I, Vec<OF>, E>
    where
        I: nom::InputIter + nom::InputTake + Offset + Slice<RangeTo<usize>> + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone,
        H: Parser<I, OH, E> + Copy + Clone,
    {
        self.terminated_x2(
            peek(recognize(not_term1).or(recognize(not_term2))),
            peek(recognize(not_term1).or(recognize(not_term2))),
        )
        .map(|(a, _)| a)
    }
}

// TODO squish these classes down by having them all implement a trait that just requires an implementation of "terminated"

impl<F, Sep> UntermT<F, Sep> {
    #[inline]
    pub fn term_by<I, OF, OS, G, OG, E>(self, term: G) -> impl Parser<I, (SepList1<OF, OS>, OG), E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        self.terminated(term)
    }

    #[inline]
    pub fn term_by_recog_alt2<I, OF, OS, G, OG, H, OH, E>(
        self,
        term1: G,
        term2: H,
    ) -> impl Parser<I, SepList1<OF, OS>, E>
    where
        I: Clone + InputLength + Offset + Slice<RangeTo<usize>>,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E>,
        H: Parser<I, OH, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        self.terminated(peek(alt((recognize(term1), recognize(term2)))))
            .map(|(a, _)| a)
    }

    #[inline]
    pub fn delim_by<I, OF, OS, L, OL, R, OR, E>(self, l: L, r: R) -> impl Parser<I, (OL, SepList1<OF, OS>, OR), E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        L: Parser<I, OL, E>,
        R: Parser<I, OR, E>,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        l.then(self.terminated(r)).map(|(a, (b, c))| (a, b, c))
    }

    // functions that relay on the specifics of the "CtrlC" parser

    #[inline]
    pub fn parened<'a, OF, OS>(self) -> impl Parser<&'a [u8], (Ctrl, SepList1<OF, OS>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
        Sep: Parser<&'a [u8], OS, ET<'a>> + Copy + Clone + Sized,
    {
        self.delim_by(ctrl('('), ctrl(')'))
    }
    #[inline]
    pub fn braced<'a, OF, OS>(self) -> impl Parser<&'a [u8], (Ctrl, SepList1<OF, OS>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
        Sep: Parser<&'a [u8], OS, ET<'a>> + Copy + Clone + Sized,
    {
        self.delim_by(ctrl('{'), ctrl('}'))
    }
    #[inline]
    pub fn ang_braced<'a, OF, OS>(self) -> impl Parser<&'a [u8], (Ctrl, SepList1<OF, OS>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
        Sep: Parser<&'a [u8], OS, ET<'a>> + Copy + Clone + Sized,
    {
        self.delim_by(ctrl('<'), ctrl('>'))
    }
    #[inline]
    pub fn bracketed<'a, OF, OS>(self) -> impl Parser<&'a [u8], (Ctrl, SepList1<OF, OS>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
        Sep: Parser<&'a [u8], OS, ET<'a>> + Copy + Clone + Sized,
    {
        self.delim_by(ctrl('['), ctrl(']'))
    }
}

impl<F, Sep> OptUntermT<F, Sep> {
    #[inline]
    pub fn term_by<I, OF, OS, G, OG, E>(self, term: G) -> impl Parser<I, (Option<SepList1<OF, OS>>, OG), E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone + Sized,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        self.terminated(term)
    }

    #[inline]
    pub fn delim_by<I, OF, OS, L, OL, R, OR, E>(
        self,
        l: L,
        r: R,
    ) -> impl Parser<I, (OL, Option<SepList1<OF, OS>>, OR), E>
    where
        I: Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        L: Parser<I, OL, E> + Copy + Clone + Sized,
        R: Parser<I, OR, E> + Copy + Clone + Sized,
        Sep: Parser<I, OS, E> + Copy + Clone + Sized,
    {
        l.then(self.term_by(r)).map(|(a, (b, c))| (a, b, c))
    }

    // functions that relay on the specifics of the "CtrlC" parser
    //
    #[inline]
    pub fn parened<'a, OF, OS>(self) -> impl Parser<&'a [u8], (Ctrl, Option<SepList1<OF, OS>>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
        Sep: Parser<&'a [u8], OS, ET<'a>> + Copy + Clone + Sized,
    {
        self.delim_by(ctrl('('), ctrl(')'))
    }
    #[inline]
    pub fn braced<'a, OF, OS>(self) -> impl Parser<&'a [u8], (Ctrl, Option<SepList1<OF, OS>>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
        Sep: Parser<&'a [u8], OS, ET<'a>> + Copy + Clone + Sized,
    {
        self.delim_by(ctrl('{'), ctrl('}'))
    }
}

impl<F> Many0<F> {
    #[inline]
    pub fn term_by<I, OF, G, OG, E>(self, term: G) -> impl Parser<I, (Vec<OF>, OG), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone + Sized,
    {
        self.terminated(term)
    }

    #[inline]
    pub fn delim_by<I, OF, L, OL, R, OR, E>(self, l: L, r: R) -> impl Parser<I, (OL, Vec<OF>, OR), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        L: Parser<I, OL, E>,
        R: Parser<I, OR, E> + Clone + Copy,
    {
        l.then(self.terminated(r)).map(|(a, (b, c))| (a, b, c))
    }

    // functions that relay on the specifics of the "CtrlC" parser

    #[inline]
    pub fn braced<'a, OF>(self) -> impl Parser<&'a [u8], (Ctrl, Vec<OF>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
    {
        self.delim_by(ctrl('{'), ctrl('}'))
    }
}

impl<F> Many1<F> {
    #[inline]
    pub fn term_by<I, OF, G, OG, E>(self, term: G) -> impl Parser<I, (Vec<OF>, OG), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        G: Parser<I, OG, E> + Copy + Clone + Sized,
    {
        self.terminated(term)
    }

    #[inline]
    pub fn delim_by<I, OF, L, OL, R, OR, E>(self, l: L, r: R) -> impl Parser<I, (OL, Vec<OF>, OR), E>
    where
        I: nom::InputIter + nom::InputTake + Clone + InputLength,
        E: ParseError<I> + ContextError<I>,
        F: Parser<I, OF, E>,
        L: Parser<I, OL, E>,
        R: Parser<I, OR, E>,
    {
        l.then(self.terminated(r)).map(|(a, (b, c))| (a, b, c))
    }

    // functions that relay on the specifics of the "CtrlC" parser

    #[inline]
    pub fn braced<'a, OF>(self) -> impl Parser<&'a [u8], (Ctrl, Vec<OF>, Ctrl), ET<'a>>
    where
        F: Parser<&'a [u8], OF, ET<'a>>,
    {
        self.delim_by(ctrl('{'), ctrl('}'))
    }
}

/// Shared implementation for parse_separated_terminated_res and
/// parse_separated_terminated. This exists so that we don't have to have an
/// unnecessary bound of FromExternalError on parse_separated_terminated.
#[inline]
pub fn uncut<'a, PO>(mut p: impl Parser<&'a [u8], PO, ET<'a>>) -> impl Parser<&'a [u8], PO, ET<'a>> {
    move |input: &'a [u8]| match p.parse(input) {
        Err(nom::Err::Error(v)) | Err(nom::Err::Failure(v)) => Err(nom::Err::Error(v)),
        v => v,
    }
}
