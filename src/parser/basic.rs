pub use crate::parser::utils::{CtrlC, CtrlN, Many0, Many1, MyParserExt, ParserExt2, Unterm, EE};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, peek},
    IResult, Parser,
};
use nom_supreme::parser_ext::ParserExt;

use crate::token::{FlatToken, TokenKind, Unspaced};

pub mod ast {
    use super::*;

    // parse something of the form "(" p1 ident ":" expr [ ".." expr ] ":" p2 ")"
    pub type MacroLoop<'a, O, O2, O3> = (O, Ident<'a>, ExprRange<'a>, O2, O3);
    pub type QualifiedName<'a> = (bool, Vec<Ident<'a>>);

    pub type AttrList<'a> = Vec<(Ident<'a>, Expr<'a>)>;

    #[derive(Debug, Clone, Copy)]
    pub enum Dir {
        Plus,
        Minus,
    }

    pub type Tok = FlatToken;
    pub type Num<'a> = &'a Tok;
    pub type Ident<'a> = &'a Tok;
    pub type StrTok<'a> = &'a Tok;
    pub type Kw<'a> = &'a Tok;

    #[derive(Debug)]
    pub enum Ctrl<'a> {
        Ctrl(&'a Tok),
        Ctrl2(&'a Tok, &'a Tok),
        Ctrl3(&'a Tok, &'a Tok, &'a Tok),
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
    pub enum UnaryOp {
        UMinus,
        Not,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
    pub enum BinaryOp {
        Plus,
        Minus,
        Mul,
        Div,
        Mod,
        LShift,
        RShift,
        ARShift,
        And,
        Or,
        Xor,
        Eq,
        NotEq,
        Leq,
        Lt,
        Geq,
        Gt,
        Concat,
        ArrayAccess,
    }

    #[derive(Debug, Clone)]
    pub enum FuncName<'a> {
        Ident(Ident<'a>),
        Int,
        Bool,
    }

    // An expression node in the AST. Children are spanned so we can generate useful runtime errors.
    #[derive(Debug, Clone)]
    pub enum Expr<'a> {
        Error,
        Num(Num<'a>),
        Ident(Ident<'a>),
        // meaning a variable or a channel that is an input to the expressions
        Unary(UnaryOp, Box<Self>),
        Binary(BinaryOp, Box<Self>, Box<Self>),
        BitField(Ident<'a>, Box<Self>, Option<Box<Self>>),
        ArrAccess(Box<Self>, Box<Self>, Option<Box<Self>>),
        Query(Box<Self>, Box<Self>, Box<Self>),

        Concat(Vec<Self>),
        Dot(Box<Self>, Ident<'a>),
        Call(FuncName<'a>, Vec<Self>),
    }

    #[derive(Debug)]
    pub enum ArrayedExprs<'a> {
        Expr(Expr<'a>),
        Braces(Vec<Self>),
        Hashes(Vec<Self>),
    }
    pub type ExprRange<'a> = (Expr<'a>, Option<Expr<'a>>);
    #[derive(Debug)]
    pub enum ExprOrStr<'a> {
        Expr(Expr<'a>),
        Str(StrTok<'a>),
    }

    #[derive(Debug)]
    pub enum PrsExpr<'a> {
        Ident(Ident<'a>),
        Paren(Box<Self>),
    }

    #[derive(Debug)]
    pub enum ArrayedExprIds<'a> {
        ExprId(ExprId<'a>),
        Braces(Vec<Self>),
        Hashes(Vec<Self>),
    }
    #[derive(Debug)]
    pub struct BaseId<'a> {
        pub ident: Ident<'a>,
        pub brackets: Vec<ExprRange<'a>>,
    }
    pub type ExprId<'a> = Vec<BaseId<'a>>;

    pub type SupplySpec<'a> = (ExprId<'a>, Option<ExprId<'a>>, Option<(ExprId<'a>, ExprId<'a>)>);
}
use ast::*;
// supply_spec: "<" expr_id [ "," expr_id ] [ "|" expr_id "," expr_id ] ">"

#[inline]
pub fn none<'a, E: EE<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], (), E> {
    map(take(0usize), |_| ()).parse(i)
}

#[inline]
pub fn num<'a, E: EE<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], Num<'a>, E> {
    tag(&[TokenKind::Num as u8][..]).map(|vs: &'a [Tok]| &vs[0]).parse(i)
}
#[inline]
pub fn ident<'a, E: EE<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], Ident<'a>, E> {
    tag(&[TokenKind::Ident as u8][..]).map(|vs: &'a [Tok]| &vs[0]).parse(i)
}
#[inline]
pub fn string<'a, E: EE<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], StrTok<'a>, E> {
    tag(&[TokenKind::Str as u8][..]).map(|vs: &'a [Tok]| &vs[0]).parse(i)
}

// TODO maybe make a struct for this (like CtrlC)
#[inline]
pub fn kw<'a, E>(s: &str) -> impl Parser<&'a [u8], Kw<'a>, E>
where
    E: EE<'a>,
{
    macro_rules! make_case {
        ($tok:expr) => {{
            const ARR: [u8; 1] = [$tok as u8];
            tag(&ARR[..]).context(stringify!(keyword $tok))
        }};
    }

    let parser = match s {
        "bool" => make_case!(TokenKind::KwBool),
        "chan" => make_case!(TokenKind::KwChan),
        "chp" => make_case!(TokenKind::KwChp),
        "dataflow" => make_case!(TokenKind::KwDataflow),
        "dataflow_cluster" => make_case!(TokenKind::KwDataflowCluster),
        "defcell" => make_case!(TokenKind::KwDefCell),
        "defchan" => make_case!(TokenKind::KwDefChan),
        "defenum" => make_case!(TokenKind::KwDefEnum),
        "defdata" => make_case!(TokenKind::KwDefData),
        "defproc" => make_case!(TokenKind::KwDefProc),
        "deftype" => make_case!(TokenKind::KwDefType),
        "else" => make_case!(TokenKind::KwElse),
        "ensures" => make_case!(TokenKind::KwEnsures),
        "enum" => make_case!(TokenKind::KwEnum),
        "export" => make_case!(TokenKind::KwExport),
        "function" => make_case!(TokenKind::KwFunction),
        "hse" => make_case!(TokenKind::KwHse),
        "initialize" => make_case!(TokenKind::KwInitialize),
        "int" => make_case!(TokenKind::KwInt),
        "ints" => make_case!(TokenKind::KwInts),
        "import" => make_case!(TokenKind::KwImport),
        "interface" => make_case!(TokenKind::KwInterface),
        "macro" => make_case!(TokenKind::KwMacro),
        "methods" => make_case!(TokenKind::KwMethods),
        "namespace" => make_case!(TokenKind::KwNamespace),
        "open" => make_case!(TokenKind::KwOpen),
        "passn" => make_case!(TokenKind::KwPassN),
        "passp" => make_case!(TokenKind::KwPassP),
        "pbool" => make_case!(TokenKind::KwPBool),
        "pint" => make_case!(TokenKind::KwPInt),
        "preal" => make_case!(TokenKind::KwPReal),
        "prs" => make_case!(TokenKind::KwPrs),
        "ptype" => make_case!(TokenKind::KwPType),
        "refine" => make_case!(TokenKind::KwRefine),
        "requires" => make_case!(TokenKind::KwRequires),
        "sizing" => make_case!(TokenKind::KwSizing),
        "skip" => make_case!(TokenKind::KwSkip),
        "spec" => make_case!(TokenKind::KwSpec),
        "template" => make_case!(TokenKind::KwTemplate),
        "timing" => make_case!(TokenKind::KwTiming),
        "transgate" => make_case!(TokenKind::KwTransgate),

        c => panic!("no such parser implemented {}", c),
    };

    map(parser, |vs: &'a [Tok]| &vs[0])
}

// TODO manually create these as well. This gives a significant
#[inline]
pub fn ctrl(c: char) -> CtrlC {
    macro_rules! make_case {
        ($c:expr) => {{
            const PADDING: u8 = 0;
            const SPACED: [u8; 3] = [TokenKind::from_ctrl($c, Unspaced::Yes) as u8, PADDING, PADDING];
            const UNSPACED: [u8; 3] = [TokenKind::from_ctrl($c, Unspaced::No) as u8, PADDING, PADDING];
            const LABEL: &'static str = stringify!($c);
            CtrlC {
                v: CtrlN::Char1,
                spaced: &SPACED,
                unspaced: &UNSPACED,
                label: LABEL,
            }
        }};
    }

    match c {
        '(' => make_case!('('),
        ')' => make_case!(')'),
        '[' => make_case!('['),
        ']' => make_case!(']'),
        '{' => make_case!('{'),
        '}' => make_case!('}'),
        ';' => make_case!(';'),
        ',' => make_case!(','),
        ':' => make_case!(':'),
        '?' => make_case!('?'),
        '!' => make_case!('!'),
        '.' => make_case!('.'),
        '+' => make_case!('+'),
        '-' => make_case!('-'),
        '*' => make_case!('*'),
        '/' => make_case!('/'),
        '=' => make_case!('='),
        '|' => make_case!('|'),
        '&' => make_case!('&'),
        '~' => make_case!('~'),
        '^' => make_case!('^'),
        '%' => make_case!('%'),
        '<' => make_case!('<'),
        '>' => make_case!('>'),
        '#' => make_case!('#'),
        '\\' => make_case!('\\'),
        '\'' => make_case!('\''),
        '@' => make_case!('@'),
        c => panic!("no such parser implemented {}", c),
    }
}

#[inline]
pub fn ctrl2(c1: char, c2: char) -> CtrlC {
    macro_rules! make_case {
        ($c1:expr, $c2:expr) => {{
            const PADDING: u8 = 0;
            const SPACED: [u8; 3] = [
                TokenKind::from_ctrl($c1, Unspaced::Yes) as u8,
                TokenKind::from_ctrl($c2, Unspaced::Yes) as u8,
                PADDING,
            ];
            const UNSPACED: [u8; 3] = [
                TokenKind::from_ctrl($c1, Unspaced::No) as u8,
                TokenKind::from_ctrl($c2, Unspaced::Yes) as u8,
                PADDING,
            ];
    const LABEL: &'static str = stringify!($c1 $c2);
            CtrlC {
                v: CtrlN::Char2,
                spaced: &SPACED,
                unspaced: &UNSPACED,
                label: LABEL,
            }
        }};
    }
    match (c1, c2) {
        ('.', '.') => make_case!('.', '.'),
        ('-', '>') => make_case!('-', '>'),
        ('<', '-') => make_case!('<', '-'),
        ('=', '=') => make_case!('=', '='),
        ('=', '>') => make_case!('=', '>'),
        ('!', '=') => make_case!('!', '='),
        ('<', '=') => make_case!('<', '='),
        ('>', '=') => make_case!('>', '='),
        ('>', '>') => make_case!('>', '>'),
        ('<', '<') => make_case!('<', '<'),
        ('*', '[') => make_case!('*', '['),
        ('[', '|') => make_case!('[', '|'),
        ('|', ']') => make_case!('|', ']'),
        (':', ':') => make_case!(':', ':'),
        ('!', '?') => make_case!('!', '?'),
        ('?', '!') => make_case!('?', '!'),
        ('[', ']') => make_case!('[', ']'),
        ('#', '>') => make_case!('#', '>'),
        ('$', '{') => make_case!('$', '{'),
        (':', '>') => make_case!(':', '>'),
        ('<', ':') => make_case!('<', ':'),
        ('+', '{') => make_case!('+', '{'),
        (':', '=') => make_case!(':', '='),
        ('!', '+') => make_case!('!', '+'),
        ('!', '-') => make_case!('!', '-'),
        ('?', '+') => make_case!('?', '+'),
        ('?', '-') => make_case!('?', '-'),
        c => panic!("no such parser implemented {:?}", c),
    }
}

#[inline]
pub fn ctrl3(c1: char, c2: char, c3: char) -> CtrlC {
    macro_rules! make_case {
        ($c1:expr, $c2:expr, $c3:expr) => {{
            const SPACED: [u8; 3] = [
                TokenKind::from_ctrl($c1, Unspaced::Yes) as u8,
                TokenKind::from_ctrl($c2, Unspaced::Yes) as u8,
                TokenKind::from_ctrl($c3, Unspaced::Yes) as u8,
            ];
            const UNSPACED: [u8; 3] = [
                TokenKind::from_ctrl($c1, Unspaced::No) as u8,
                TokenKind::from_ctrl($c2, Unspaced::Yes) as u8,
                TokenKind::from_ctrl($c3, Unspaced::Yes) as u8,
            ];
    const LABEL: &'static str = stringify!($c1 $c2 $c3);
            CtrlC {
                v: CtrlN::Char3,
                spaced: &SPACED,
                unspaced: &UNSPACED,
                label: LABEL,
            }
        }};
    }
    match (c1, c2, c3) {
        ('.', '.', '.') => make_case!('.', '.', '.'),
        ('>', '>', '>') => make_case!('>', '>', '>'),
        ('<', '<', '<') => make_case!('<', '<', '<'),
        ('=', '=', '=') => make_case!('=', '=', '='),
        ('!', '=', '=') => make_case!('!', '=', '='),
        c => panic!("no such parser implemented {:?}", c),
    }
}

pub fn qualified_name<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], QualifiedName<'a>, E> {
    let leading_colons = ctrl2(':', ':').p().opt().map(|v| v.is_some());
    leading_colons
        .then(ident.list1_sep_by(ctrl2(':', ':')).p())
        .context("qualified name")
        .parse(i)
}

// attr_list: "[" { ID "=" expr ";" }** "]"
pub fn attr_list<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], AttrList<'a>, E> {
    let one_attr = ident.then_ignore(ctrl('=')).then(expr);
    one_attr.list1_sep_by(ctrl(';')).bracketed().parse(i)
}

pub fn dir<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Dir, E> {
    alt((ctrl('+').p().value(Dir::Plus), ctrl('-').p().value(Dir::Minus))).parse(i)
}

fn unary_rec<'a, F, E: EE<'a>>(expr: F) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, E>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, E>,
{
    move |i| {
        // 'Atoms' are expressions that contain no ambiguity
        let concat = map((&expr).list1_sep_by(ctrl(',')).braced(), |es| Expr::Concat(es));

        let bitfield = ident
            .then((&expr).then(ctrl2('.', '.').ignore_then(&expr).opt()).braced())
            .map(|(a, (b, c))| Expr::BitField(a, Box::new(b), c.map(Box::new)));

        let func_name = alt((
            kw("int").map(|_| FuncName::Int),
            kw("bool").map(|_| FuncName::Bool),
            ident.map(FuncName::Ident),
        ));
        let func_call = func_name
            .then((&expr).list1_sep_by(ctrl(',')).parened()) // TODO make this be list0_sep_by?
            .map(|(name, args)| Expr::Call(name, args));

        let atom = alt((
            num.map(Expr::Num),
            // bitfield and func_call go before "local"
            bitfield,
            func_call,
            ident.map(Expr::Ident), // TODO support qualified names?
            (&expr).parened(),
            concat,
        ));

        // https://en.cppreference.com/w/c/language/operator_precedence

        // apply function calls, array accesses, and dot operators
        enum AccessKind<'a> {
            // Func(Vec<Spanned<Expr>>),
            Arr(Expr<'a>, Option<Expr<'a>>),
            Dot(Ident<'a>),
        }

        let arr_access = (&expr)
            .then(ctrl2('.', '.').ignore_then(&expr).opt())
            .bracketed()
            .map(|(i1, i2)| AccessKind::Arr(i1, i2));
        let dot_access = ctrl('.').ignore_then(ident).map(AccessKind::Dot);

        let arr_or_dot_access = alt((
            peek(ctrl('[')).ignore_then_cut(arr_access),
            peek(ctrl('.')).ignore_then_cut(dot_access),
        ));

        let access = atom
            .then(arr_or_dot_access.many0().term_by_peek_not_alt2(ctrl('['), ctrl('.')))
            .map(|(lhs, accs)| {
                accs.into_iter().fold(lhs, |lhs, access| match access {
                    AccessKind::Arr(i, j) => Expr::ArrAccess(Box::new(lhs), Box::new(i), j.map(Box::new)),
                    AccessKind::Dot(iden) => Expr::Dot(Box::new(lhs), iden),
                })
            });

        // Unary operators have precidence 2
        let op = ctrl('-')
            .p()
            .value(UnaryOp::UMinus)
            .or(ctrl('~').p().value(UnaryOp::Not));

        // This is an ok use of the nom version of "many0", because all of the things being parsed are 1 token long
        nom::multi::many0(op)
            .then(access)
            .map(|(ops, a)| ops.iter().rev().fold(a, |e, op| Expr::Unary(*op, Box::new(e))))
            .parse(i)
    }
}

fn binary_rec<'a, F, G, E: EE<'a>>(expr: F, infix_binary_ops: G) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, E>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, E>,
    G: Fn(&'a [u8]) -> IResult<&'a [u8], BinaryOp, E>,
{
    move |i| {
        // In order to make it compile in a reasonable amount of time, we do binary operator parsing in two steps.
        // First we parse a chain of binary operators, and then we parse the precidence within the chains

        unary_rec(&expr)
            .then(
                (&infix_binary_ops)
                    .then(unary_rec(&expr))
                    .many0()
                    .term_by_peek_not(&infix_binary_ops),
            )
            .map(|(n0, ns)| {
                // this bit of code is gross as it uses a mutable algorithm. It scans over the list [node0, nodes[0], nodes[1], ...]
                // and combines any pair of nodes with one of the specified operations in between
                let fold_nodes_with_ops = |node0, nodes: Vec<(BinaryOp, Expr<'a>)>, ops: Vec<BinaryOp>| {
                    let mut result =
                        nodes
                            .into_iter()
                            .fold(vec![(BinaryOp::Mul /*dummy value*/, node0)], |mut ls, (r_op, r)| {
                                if ops.contains(&r_op) {
                                    let (l_op, l) = ls.pop().unwrap();
                                    let new_node: Expr = Expr::Binary(r_op, Box::new(l), Box::new(r));
                                    ls.push((l_op, new_node));
                                } else {
                                    ls.push((r_op, r));
                                }
                                ls
                            });
                    let n0 = result.remove(0);
                    (n0.1, result)
                };

                let (n0, ns) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::Mul, BinaryOp::Div, BinaryOp::Mod]);
                let (n0, ns) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::Plus, BinaryOp::Minus]);
                let (n0, ns) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::LShift, BinaryOp::RShift, BinaryOp::ARShift]);
                let (n0, ns) =
                    fold_nodes_with_ops(n0, ns, vec![BinaryOp::Lt, BinaryOp::Leq, BinaryOp::Gt, BinaryOp::Geq]);
                let (n0, ns) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::Eq, BinaryOp::NotEq]);
                let (n0, ns) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::And]);
                let (n0, ns) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::Xor]);
                let (combined, rest) = fold_nodes_with_ops(n0, ns, vec![BinaryOp::Or]);

                assert_eq!(rest.len(), 0);
                combined
            })
            .parse(i)
    }
}

fn query_rec<'a, F, G, E: EE<'a>>(expr: F, infix_binary_ops: G) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, E>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, E>,
    G: Fn(&'a [u8]) -> IResult<&'a [u8], BinaryOp, E>,
{
    move |i| {
        // handles `a ? b : c ? d : e` as `a ? b : (c ? d : e)`
        binary_rec(&expr, &infix_binary_ops)
            .then(
                (ctrl('?')
                    .ignore_then(binary_rec(&expr, &infix_binary_ops))
                    .then_ignore(ctrl(':'))
                    .then(binary_rec(&expr, &infix_binary_ops)))
                .many0()
                .term_by_peek_not(ctrl('?')),
            )
            .map(|(sel0, nodes)| {
                let nodes: Vec<(Expr<'a>, Expr<'a>)> = nodes;
                let (v1s, mut sels): (Vec<Expr<'a>>, Vec<Expr<'a>>) = nodes.into_iter().unzip();
                if sels.len() == 0 {
                    sel0
                } else {
                    let final_v2 = sels.pop().unwrap();
                    sels.insert(0usize, sel0);

                    assert_eq!(sels.len(), v1s.len());
                    sels.into_iter()
                        .rev()
                        .zip(v1s.into_iter().rev())
                        .fold(final_v2, |v2, (sel, v1)| {
                            Expr::Query(Box::new(sel), Box::new(v1), Box::new(v2))
                        })
                }
            })
            .parse(i)
    }
}

fn infix_binary_ops<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], BinaryOp, E> {
    alt((
        map(ctrl('*'), |_| BinaryOp::Mul),
        map(ctrl('/'), |_| BinaryOp::Div),
        map(ctrl('%'), |_| BinaryOp::Div),
        map(ctrl('+'), |_| BinaryOp::Plus),
        map(ctrl('-'), |_| BinaryOp::Minus),
        map(ctrl2('>', '>'), |_| BinaryOp::RShift),
        map(ctrl2('<', '<'), |_| BinaryOp::LShift),
        map(ctrl3('>', '>', '>'), |_| BinaryOp::ARShift),
        map(ctrl('>'), |_| BinaryOp::Gt),
        map(ctrl('<'), |_| BinaryOp::Lt),
        map(ctrl2('>', '='), |_| BinaryOp::Geq),
        map(ctrl2('<', '='), |_| BinaryOp::Leq),
        map(ctrl('='), |_| BinaryOp::Eq),
        map(ctrl2('!', '='), |_| BinaryOp::NotEq),
        map(ctrl('&'), |_| BinaryOp::And),
        map(ctrl('^'), |_| BinaryOp::Xor),
        map(ctrl('|'), |_| BinaryOp::Or),
    ))(i)
}

fn infix_binary_ops_no_gt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], BinaryOp, E> {
    alt((
        map(ctrl('*'), |_| BinaryOp::Mul),
        map(ctrl('/'), |_| BinaryOp::Div),
        map(ctrl('%'), |_| BinaryOp::Div),
        map(ctrl('+'), |_| BinaryOp::Plus),
        map(ctrl('-'), |_| BinaryOp::Minus),
        // map(ctrl2('>', '>'), |_| BinaryOp::RShift),
        map(ctrl2('<', '<'), |_| BinaryOp::LShift),
        // map(ctrl3('>', '>', '>'), |_| BinaryOp::ARShift),
        // map(ctrl('>'), |_| BinaryOp::Gt),
        map(ctrl('<'), |_| BinaryOp::Lt),
        map(ctrl2('>', '='), |_| BinaryOp::Geq),
        map(ctrl2('<', '='), |_| BinaryOp::Leq),
        map(ctrl('='), |_| BinaryOp::Eq),
        map(ctrl2('!', '='), |_| BinaryOp::NotEq),
        map(ctrl('&'), |_| BinaryOp::And),
        map(ctrl('^'), |_| BinaryOp::Xor),
        map(ctrl('|'), |_| BinaryOp::Or),
    ))(i)
}

pub fn expr<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Expr<'a>, E> {
    query_rec(expr, infix_binary_ops).context("expr").parse(i)
}

pub fn expr_no_gt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Expr<'a>, E> {
    query_rec(expr, infix_binary_ops_no_gt).context("expr no gt").parse(i)
}

// arrayed_exprs: { array_term "#" }*
// array_term: {excl}
//            "{" { arrayed_exprs "," }* "}"
//           | expr
pub fn arrayed_exprs_no_gt<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ArrayedExprs<'a>, E> {
    let expr = expr_no_gt
        .map(ArrayedExprs::Expr)
        .or(arrayed_exprs.list1_sep_by(ctrl(',')).braced().map(ArrayedExprs::Braces));
    let braces = arrayed_exprs.list1_sep_by(ctrl(',')).braced().map(ArrayedExprs::Braces);

    expr.or(braces)
        .list1_sep_by(ctrl('#'))
        .p()
        .map(ArrayedExprs::Hashes)
        .context("arrayed exprs no gt")
        .parse(i)
}
pub fn arrayed_exprs<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ArrayedExprs<'a>, E> {
    let expr = expr.map(ArrayedExprs::Expr);
    let braces = arrayed_exprs.list1_sep_by(ctrl(',')).braced().map(ArrayedExprs::Braces);

    expr.or(braces)
        .list1_sep_by(ctrl('#'))
        .p()
        .map(ArrayedExprs::Hashes)
        .context("arrayed exprs")
        .parse(i)
}

pub fn expr_range<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ExprRange<'a>, E> {
    let opt_dotted_part = alt((
        peek(ctrl('.'))
            .ignore_then_cut(ctrl2('.', '.').ignore_then(expr))
            .map(|v| Some(v)),
        none.map(|_| None),
    ));
    expr.then(opt_dotted_part).context("expr range").parse(i)
}

pub fn bracketed_spare_ranges<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], Vec<ExprRange<'a>>, E> {
    // first look ahead to see if there are brackets. If there are, then parse brackets until there arent brackets any more
    // TODO is all of this needed
    expr_range.bracketed().many0().term_by_peek_not(ctrl('[')).parse(i)
}

pub fn expr_or_str<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ExprOrStr<'a>, E> {
    expr.map(ExprOrStr::Expr).or(string.map(ExprOrStr::Str)).parse(i)
}

pub fn prs_expr<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], PrsExpr<'a>, E> {
    map(ident, |i| PrsExpr::Ident(i))(i)
}

pub fn arrayed_expr_ids<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ArrayedExprIds<'a>, E> {
    // arrayed_expr_ids: { lhs_array_term "#" }*
    // lhs_array_term: {excl}
    //                "{" { arrayed_expr_ids "," }* "}"
    //               | expr_id

    let expr_id = expr_id.map(ArrayedExprIds::ExprId);
    let braces = arrayed_expr_ids
        .list1_sep_by(ctrl(','))
        .braced()
        .map(ArrayedExprIds::Braces);

    expr_id
        .or(braces)
        .list1_sep_by(ctrl('#'))
        .p()
        .map(ArrayedExprIds::Hashes)
        .context("arrayed expr ids")
        .parse(i)
}

// xsparse_range: xsparse_one_range xsparse_range
//               | xsparse_one_range
// xsparse_one_range: "[" !noreal expr [ ".." expr ] "]"
// base_id: ID [ xsparse_range ]
// expr_id: { base_id "." }*

pub fn base_id<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], BaseId<'a>, E> {
    // first look ahead to see if there are brackets. If there are, then parse brackets until there arent brackets any more
    ident
        .then(bracketed_spare_ranges.opt().map(|v| match v {
            Some(v) => v,
            None => Vec::new(),
        }))
        .map(|(ident, brackets)| BaseId { ident, brackets })
        .parse(i)
}

pub fn expr_id<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], ExprId<'a>, E> {
    base_id.list1_sep_by(ctrl('.')).p().context("expr id").parse(i)
}

pub fn supply_spec<'a, E: EE<'a>>(i: &'a [u8]) -> IResult<&'a [u8], SupplySpec<'a>, E> {
    expr_id
        .then(ctrl(',').ignore_then(expr_id).opt())
        .then(
            ctrl('|')
                .ignore_then(expr_id)
                .then_ignore(ctrl(','))
                .then(expr_id)
                .opt(),
        )
        .ang_braced()
        .map(|((a, b), c)| (a, b, c))
        .parse(i)
}
