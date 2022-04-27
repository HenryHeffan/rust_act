use crate::parser::ast::{SepList1, Tok};
use crate::{FlatToken, Token, WhitespaceKind};
use itertools::EitherOrBoth::*;
use itertools::Itertools;
use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use std::mem::swap;
use std::str;

// https://docs.rs/pretty/latest/pretty/

#[derive(Clone, Debug)]
pub enum PraSpaceKind {
    Space,
    Line,
    SoftLine,
    HardLine,
    Line_,
    SoftLine_,
}

#[derive(Clone)]
pub enum Pra {
    Nil,
    S(PraSpaceKind),
    Tok(usize), // the index of the token
    Concat(Vec<Self>),
    Group(Box<Self>),
    Nest(Box<Self>, usize),
    // This is e.g. a base_item or a chp_item, etc. It is a thing that can have extra new lines in front of it without strange behavior, and which should preserve double spaces if they exist
    DelimitedChunkConcat(bool, Vec<PraChunk>, Option<Box<Self>>, usize),
}

#[derive(Clone)]
pub struct PraChunk(Pra); // a thing that is meant to live on one line or one set of lines

pub trait PrAble {
    fn pr(&self) -> Pra;
}

impl<'a> PrAble for Pra {
    fn pr(&self) -> Pra {
        self.clone()
    }
}

impl<T: PrAble> PrAble for &T
where
    T: PrAble,
{
    fn pr(&self) -> Pra {
        let t: &T = self;
        t.pr()
    }
}

pub fn ptr_from_tok(c: &Tok) -> usize {
    c as *const u8 as usize
}

impl Pra {
    pub fn from_tok(c: &Tok) -> Pra {
        Pra::Tok(ptr_from_tok(c))
    }
    pub fn group(self) -> Pra {
        Pra::Group(Box::new(self))
    }
    pub fn nest(self, indent: usize) -> Pra {
        Pra::Nest(Box::new(self), indent)
    }
    pub fn chunk(self) -> PraChunk {
        PraChunk(self)
    }
}

pub fn nil() -> Pra {
    Pra::Nil
}
pub fn space() -> Pra {
    Pra::S(PraSpaceKind::Space)
}
pub fn line() -> Pra {
    Pra::S(PraSpaceKind::Line)
}
pub fn soft_line() -> Pra {
    Pra::S(PraSpaceKind::SoftLine)
}
pub fn hard_line() -> Pra {
    Pra::S(PraSpaceKind::HardLine)
}
pub fn line_() -> Pra {
    Pra::S(PraSpaceKind::Line_)
}
pub fn soft_line_() -> Pra {
    Pra::S(PraSpaceKind::SoftLine_)
}

pub trait PrAbleTuple {
    fn pr_tup(&self) -> Pra;
}

macro_rules! tuple_impls {
    ( $( $name:ident )+ ) => {
        impl<$($name: PrAble,)+> PrAbleTuple for ($($name,)+)
        {
            #[allow(non_snake_case)]
            #[inline]
            fn pr_tup(&self) -> Pra {
                let ($($name,)+) = self;
                Pra::Concat(vec![$($name.pr(),)+])
            }
        }
    };
}

tuple_impls! { A }
tuple_impls! { A B }
tuple_impls! { A B C }
tuple_impls! { A B C D }
tuple_impls! { A B C D E }
tuple_impls! { A B C D E F }
tuple_impls! { A B C D E F G }
tuple_impls! { A B C D E F G H }
tuple_impls! { A B C D E F G H I }
tuple_impls! { A B C D E F G H I J }
tuple_impls! { A B C D E F G H I J K }
tuple_impls! { A B C D E F G H I J K L }
tuple_impls! { A B C D E F G H I J K L M }
tuple_impls! { A B C D E F G H I J K L M N }
tuple_impls! { A B C D E F G H I J K L M N O }
tuple_impls! { A B C D E F G H I J K L M N O P }
tuple_impls! { A B C D E F G H I J K L M N O P Q }
tuple_impls! { A B C D E F G H I J K L M N O P Q R }
tuple_impls! { A B C D E F G H I J K L M N O P Q R S }
tuple_impls! { A B C D E F G H I J K L M N O P Q R S T }

pub fn concat<T: PrAbleTuple>(t: T) -> Pra {
    t.pr_tup()
}
pub fn group<T: PrAbleTuple>(t: T) -> Pra {
    concat(t).group()
}

pub fn concat_map<O, F: Fn(&O) -> Pra>(l: &Vec<O>, f: F) -> Pra {
    Pra::Concat(l.into_iter().map(f).collect_vec())
}
pub fn concat_interweave(items: Vec<Pra>, seps: Vec<Pra>, sep: Pra) -> Pra {
    let zipped = items.into_iter().zip_longest(seps.into_iter()).map(|x| match x {
        Both(x, y) => concat((x, y)),
        Left(x) => x,
        Right(_) => panic!(), // there shouldnt be more seperators than items
    });
    Pra::Concat(itertools::intersperse(zipped, sep).collect::<Vec<_>>())
}

pub fn concat_sep1<O: PrAble, S: PrAble>(l: &SepList1<O, S>, sep: Pra) -> Pra {
    let items = l.items.iter().map(|v| v.pr()).collect_vec();
    let seps = l.seps.iter().map(|v| v.pr()).collect_vec();
    concat_interweave(items, seps, sep)
}

pub fn zip_map2_sep1<O, S: PrAble, T, F: Fn(&O, Pra) -> T>(l: &SepList1<O, S>, term: Pra, f: F) -> Vec<T> {
    let seps = l.seps.iter().map(|v| v.pr()).collect_vec();
    l.items
        .iter()
        .zip_eq(seps.into_iter().chain(vec![term]))
        .map(|(item, sep)| f(item, sep))
        .collect_vec()
}
pub fn zip_map_sep1<O, S: PrAble, F: Fn(&O) -> Pra>(l: &SepList1<O, S>, term: Pra, f: F) -> Vec<Pra> {
    let items = l.items.iter().map(|v| f(v)).collect_vec();
    let seps = l.seps.iter().map(|v| v.pr()).collect_vec();
    items
        .into_iter()
        .zip_eq(seps.into_iter().chain(vec![term]))
        .map(|(item, sep)| concat((item, sep)))
        .collect_vec()
}
pub fn zip_map_sep1_as_chunks<O, S: PrAble, F: Fn(&O) -> Pra>(l: &SepList1<O, S>, term: Pra, f: F) -> Vec<PraChunk> {
    zip_map_sep1(l, term, f).into_iter().map(|v| v.chunk()).collect_vec()
}
pub fn zip_sep1_as_chunks<O: PrAble, S: PrAble>(l: &SepList1<O, S>, term: Pra) -> Vec<PraChunk> {
    zip_map_sep1_as_chunks(l, term, |o| o.pr())
}

pub fn concat_map_sep1<O, S: PrAble, F: Fn(&O) -> Pra>(l: &SepList1<O, S>, sep: Pra, f: F) -> Pra {
    let items = l.items.iter().map(|v| f(v).pr()).collect_vec();
    let seps = l.seps.iter().map(|v| v.pr()).collect_vec();
    concat_interweave(items, seps, sep)
}

pub fn concat_vec<O: PrAble>(l: &Vec<O>, sep: Pra) -> Pra {
    Pra::Concat(itertools::intersperse(l.into_iter().map(|v| v.pr()), sep).collect::<Vec<_>>())
}
pub fn concat_chunks(require_initial_nl: bool, l: Vec<PraChunk>, rbrace: Option<Pra>, nest_amt: usize) -> Pra {
    Pra::DelimitedChunkConcat(require_initial_nl, l, rbrace.map(Box::new), nest_amt)
}
pub fn concat_map_vec<O, F: Fn(&O) -> Pra>(l: &Vec<O>, sep: Pra, f: F) -> Pra {
    Pra::Concat(itertools::intersperse(l.into_iter().map(|v| f(v)), sep).collect::<Vec<_>>())
}

#[derive(Clone, Debug)]
enum Comment {
    NewLine,
    // the comment string, NOT INCLUDING the final new line. This will be followed by a Space(1)
    LineComment(String),
    BlockComment(String),
}

#[derive(Clone, Debug)]
struct EvaledPraChunk(Vec<Comment>, EvaledPra); // a thing that is meant to live on one line or one set of lines
#[derive(Clone, Debug)]
enum EvaledPra {
    S(PraSpaceKind),
    Tok(Vec<Comment>, String), // the index of the token
    Concat(Vec<Self>),
    Group(Box<Self>),
    Nest(Box<Self>, usize),
    DelimitedChunkConcat(bool, Vec<EvaledPraChunk>, Vec<Comment>, Option<Box<Self>>, usize),
}

impl Pra {
    // remove all the nulls, and copy tokens into strings
    fn decode<'a>(self, ft_init_loc: usize, tokens: &'a Vec<Token<'a>>) -> Option<EvaledPra> {
        match self {
            Pra::Nil => None,
            Pra::S(v) => Some(EvaledPra::S(v)),
            Pra::Concat(items) => {
                let mut new_items = Vec::new();
                for item in items {
                    let g = item.decode(ft_init_loc, tokens);
                    match g {
                        Some(g) => new_items.push(g),
                        None => (),
                    }
                }
                match new_items.len() > 0 {
                    true => Some(EvaledPra::Concat(new_items)),
                    false => None,
                }
            }
            Pra::Group(g) => g.decode(ft_init_loc, tokens).map(Box::new).map(EvaledPra::Group),
            Pra::Nest(g, amt) => g
                .decode(ft_init_loc, tokens)
                .map(Box::new)
                .map(|v| EvaledPra::Nest(v, amt)),
            Pra::DelimitedChunkConcat(require_initial_nl, g, rparen, amt) => {
                Some(EvaledPra::DelimitedChunkConcat(
                    require_initial_nl,
                    g.into_iter()
                        // a chunk should never be empty, and so should never be converted to nil
                        .map(|PraChunk(g)| EvaledPraChunk(Vec::new(), g.decode(ft_init_loc, tokens).unwrap()))
                        .collect_vec(),
                    Vec::new(),
                    rparen.map(|v| Box::new(v.decode(ft_init_loc, tokens).unwrap())), // true because we want to strip leading spacers
                    amt,
                ))
            }
            Pra::Tok(ft_loc) => {
                let tok = &tokens[ft_loc - ft_init_loc];
                let tok_text = tok.str_.to_string();

                let comments = tok
                    .leading_space
                    .iter()
                    .map(|v| match v.0 {
                        WhitespaceKind::UntermBlockComment => panic!(),
                        WhitespaceKind::Space => (0..v.1.matches('\n').count()).map(|_| Comment::NewLine).collect_vec(),
                        WhitespaceKind::LineComment => {
                            vec![Comment::LineComment(v.1.to_string())]
                        }
                        WhitespaceKind::BlockComment => vec![Comment::BlockComment(v.1.to_string())],
                    })
                    .flatten()
                    .collect_vec();

                Some(EvaledPra::Tok(comments, tok_text))
            }
        }
    }
}

impl EvaledPra {
    fn steal_leading_comments(&mut self) -> Option<Vec<Comment>> {
        match self {
            EvaledPra::S(_) => None,
            EvaledPra::Tok(c, _) => {
                let mut comm = Vec::new();
                swap(c, &mut comm);
                Some(comm)
            }
            EvaledPra::Concat(items) => {
                for item in items {
                    match item.steal_leading_comments() {
                        Some(v) => return Some(v),
                        None => {}
                    }
                }
                None
            }
            EvaledPra::Group(g) => g.steal_leading_comments(),
            EvaledPra::Nest(g, _) => g.steal_leading_comments(),
            EvaledPra::DelimitedChunkConcat(_, _, _, _, _) => panic!(),
        }
    }
    fn shift_comments(&mut self) {
        match self {
            EvaledPra::S(_) => (),
            EvaledPra::Tok(_, _) => (),
            EvaledPra::Concat(items) => items.into_iter().for_each(|v| v.shift_comments()),
            EvaledPra::Group(g) => g.shift_comments(),
            EvaledPra::Nest(g, _) => g.shift_comments(),
            EvaledPra::DelimitedChunkConcat(_, items, comments, rparen, _) => {
                items.into_iter().for_each(|v| v.1.shift_comments());
                items.into_iter().for_each(|mut item| {
                    item.0 = item.1.steal_leading_comments().unwrap_or(Vec::new());
                });
                match rparen {
                    Some(rparen) => *comments = rparen.steal_leading_comments().unwrap_or(Vec::new()),
                    None => {}
                }
            }
        }
    }

    fn pretty<'b, D, A: Clone>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
    {
        match self {
            EvaledPra::S(sk) => match sk {
                PraSpaceKind::Space => allocator.space(),
                PraSpaceKind::Line => allocator.line(),
                PraSpaceKind::SoftLine => allocator.softline(),
                PraSpaceKind::HardLine => allocator.hardline(),
                PraSpaceKind::Line_ => allocator.line_(),
                PraSpaceKind::SoftLine_ => allocator.softline_(),
            },
            EvaledPra::Tok(comments, tok_text) => {
                // strip out all new-lines after LineComment block (as these are now implicit), and strip out repeated newline comments. NewLine will now represent "soft" new-lines
                let comments = comments.clone().into_iter().fold(Vec::new(), |mut v, x| {
                    if v.len() == 0 {
                        v.push(x);
                    } else if matches!(&v[v.len() - 1], Comment::NewLine) && matches!(x, Comment::NewLine) {
                    } else if matches!(&v[v.len() - 1], Comment::LineComment(_)) && matches!(x, Comment::NewLine) {
                    } else {
                        v.push(x)
                    }
                    v
                });

                let comments = match comments.len() == 1 && matches!(comments[0], Comment::NewLine) {
                    true => Vec::new(),
                    false => comments,
                };

                let comments = allocator.concat(comments.into_iter().map(|v| match v {
                    Comment::NewLine => allocator.softline(),
                    Comment::LineComment(s) => allocator.text(s).append(allocator.hardline()),
                    Comment::BlockComment(s) => allocator.text(s),
                }));

                comments.append(tok_text)
                // tok_text.group()
            }
            EvaledPra::Concat(items) => {
                assert!(items.len() > 0); // otherwise we need to remove all dead branches from the tree
                allocator.concat(items.iter().map(|v| v.pretty(allocator)))
            }
            EvaledPra::Group(g) => g.pretty(allocator).group(),
            EvaledPra::Nest(g, amt) => g.pretty(allocator).nest(*amt as isize),
            EvaledPra::DelimitedChunkConcat(first_require_initial_nl, items, final_comments, rparen, nest_amt) => {
                assert!(items.len() > 0); // otherwise we need to remove all dead branches from the tree

                let strip_3nl = |comments: Vec<Comment>| {
                    comments.into_iter().fold(Vec::new(), |mut v, x| {
                        if v.len() <= 1 {
                            v.push(x);
                        } else {
                            match (
                                matches!(&v[v.len() - 2], Comment::NewLine),
                                matches!(&v[v.len() - 1], Comment::NewLine),
                                matches!(x, Comment::NewLine),
                            ) {
                                (true, true, true) => {}
                                (_, _, _) => v.push(x),
                            };
                        }
                        v
                    })
                };

                let items = allocator.concat(items.iter().enumerate().map(|(idx, chunk)| {
                    let is_first = idx == 0;
                    let mut comments = chunk.0.clone();
                    // if there is not a new line at the end of the comments, add one
                    if comments.len() > 0 && !matches!(comments.last().unwrap(), Comment::NewLine) {
                        comments.push(Comment::NewLine)
                    }

                    if is_first {
                        if *first_require_initial_nl {
                            while comments.len() > 1
                                && matches!(comments[0], Comment::NewLine)
                                && matches!(comments[1], Comment::NewLine)
                            {
                                comments.remove(0);
                            }
                            if comments.len() == 0 {
                                comments.push(Comment::NewLine);
                            }
                        } else {
                            while comments.len() > 0 && matches!(comments[0], Comment::NewLine) {
                                comments.remove(0);
                            }
                        }
                    }

                    // strip out any time there are more than two consecutive new-lines
                    let comments = strip_3nl(comments);

                    let leading_space = match comments.len() > 0 && !matches!(comments[0], Comment::NewLine) {
                        true => allocator.space(),
                        false => allocator.nil(),
                    };

                    let comments = allocator.concat(comments.into_iter().map(|v| match v {
                        Comment::NewLine => allocator.hardline(),
                        Comment::LineComment(s) | Comment::BlockComment(s) => allocator.text(s),
                    }));

                    leading_space.append(comments).append(chunk.1.pretty(allocator))
                }));

                let mut final_comments = final_comments.clone();
                // strip of all the final new-lines
                while final_comments.len() > 0 && matches!(final_comments.last().unwrap(), Comment::NewLine) {
                    final_comments.pop();
                }

                // strip out any time there are more than two consecutive new-lines
                let final_comments = strip_3nl(final_comments);

                let final_comments = allocator.concat(final_comments.into_iter().map(|v| match v {
                    Comment::NewLine => allocator.hardline(),
                    Comment::LineComment(s) | Comment::BlockComment(s) => allocator.text(s),
                }));

                let rparen = rparen
                    .as_ref()
                    .map_or(allocator.nil(), |v| allocator.hardline().append(v.pretty(allocator)));
                items.append(final_comments).nest(*nest_amt as isize).append(rparen)
            }
        }
    }
}

pub fn as_pretty(ast: Pra, flat_tokens: &Vec<FlatToken>, tokens: &Vec<Token>, width: usize) -> Option<String> {
    let allocator = BoxAllocator;
    let mut mem = Vec::new();
    assert!(matches!(ast, Pra::DelimitedChunkConcat(_, _, _, _)));
    let mut ast = ast.decode(flat_tokens.as_ptr() as usize, tokens).unwrap();
    ast.shift_comments();
    let result = ast
        .pretty::<_, ()>(&allocator)
        .1
        .render(width, &mut mem)
        .map(|()| {
            let res = str::from_utf8(&mem).unwrap_or("expected utf8 string");
            res.to_string()
        })
        .ok();
    result
}