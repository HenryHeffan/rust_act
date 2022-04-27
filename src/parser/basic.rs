pub use crate::parser::utils::{uncut, CtrlC, CtrlN, KwC, Many0, Many1, MyParserExt, ParserExt2, Unterm, ET};
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{not, peek},
    IResult, Parser,
};
use nom_supreme::parser_ext::ParserExt;

use crate::token::{FlatToken, TokenKind, Unspaced};

pub mod ast {
    use super::*;
    use crate::parser::utils::SepList1;

    pub type Tok = FlatToken;
    #[derive(Debug, Copy, Clone)]
    pub struct Num<'a>(pub &'a Tok);
    #[derive(Debug, Copy, Clone)]
    pub struct Ident<'a>(pub &'a Tok);
    #[derive(Debug, Copy, Clone)]
    pub struct StrTok<'a>(pub &'a Tok);
    #[derive(Debug, Copy, Clone)]
    pub struct Kw<'a>(pub &'a Tok);

    #[derive(Debug, Copy, Clone)]
    pub enum Ctrl<'a> {
        Ctrl(&'a Tok),
        Ctrl2(&'a Tok, &'a Tok),
        Ctrl3(&'a Tok, &'a Tok, &'a Tok),
    }
    pub type CtrlEquals<'a> = Ctrl<'a>;
    pub type CtrlLParen<'a> = Ctrl<'a>;
    pub type CtrlRParen<'a> = Ctrl<'a>;
    pub type CtrlLBracket<'a> = Ctrl<'a>;
    pub type CtrlRBracket<'a> = Ctrl<'a>;
    pub type CtrlLBrace<'a> = Ctrl<'a>;
    pub type CtrlRBrace<'a> = Ctrl<'a>;
    pub type CtrlLAngBrace<'a> = Ctrl<'a>;
    pub type CtrlRAngBrace<'a> = Ctrl<'a>;
    pub type CtrlDot<'a> = Ctrl<'a>;
    pub type CtrlDotDot<'a> = Ctrl<'a>;
    pub type CtrlColon<'a> = Ctrl<'a>;
    pub type CtrlColonColon<'a> = Ctrl<'a>;
    pub type CtrlComma<'a> = Ctrl<'a>;
    pub type CtrlSemi<'a> = Ctrl<'a>;
    pub type CtrlHash<'a> = Ctrl<'a>; // #
    pub type CtrlLArrow<'a> = Ctrl<'a>; // ->
    pub type CtrlAtSign<'a> = Ctrl<'a>;
    pub type CtrlVBar<'a> = Ctrl<'a>; // |
    pub type CtrlQMark<'a> = Ctrl<'a>;
    pub type CtrlStar<'a> = Ctrl<'a>; // *

    #[derive(Debug)]
    pub struct QualifiedName<'a>(
        pub Option<CtrlColonColon<'a>>,
        pub SepList1<Ident<'a>, CtrlColonColon<'a>>,
    );

    #[derive(Debug)]
    pub struct BracketedAttrList<'a>(
        pub CtrlLBracket<'a>,
        pub SepList1<(Ident<'a>, CtrlEquals<'a>, Expr<'a>), CtrlSemi<'a>>,
        pub CtrlRBracket<'a>,
    );

    #[derive(Debug, Clone, Copy)]
    pub enum Dir {
        Plus,
        Minus,
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
        Int(Kw<'a>),
        Bool(Kw<'a>),
    }

    // An expression node in the AST. Children are spanned so we can generate useful runtime errors.
    #[derive(Debug, Clone)]
    pub enum Expr<'a> {
        Num(Num<'a>),
        Ident(Ident<'a>),
        // meaning a variable or a channel that is an input to the expressions
        Unary(UnaryOp, Ctrl<'a>, Box<Self>),
        Binary(BinaryOp, Ctrl<'a>, Box<Self>, Box<Self>),
        BitField(
            Ident<'a>,
            CtrlLBrace<'a>,
            Box<Self>,
            Option<(CtrlDotDot<'a>, Box<Self>)>,
            CtrlRBrace<'a>,
        ),
        ArrAccess(
            Box<Self>,
            CtrlLBracket<'a>,
            Box<Self>,
            Option<(CtrlDotDot<'a>, Box<Self>)>,
            CtrlRBracket<'a>,
        ),
        Query(Box<Self>, CtrlQMark<'a>, Box<Self>, CtrlColon<'a>, Box<Self>),
        Concat(CtrlLBrace<'a>, SepList1<Self, CtrlComma<'a>>, CtrlRBrace<'a>),
        Dot(Box<Self>, CtrlDot<'a>, Ident<'a>),
        Call(
            FuncName<'a>,
            CtrlLParen<'a>,
            SepList1<Self, CtrlComma<'a>>,
            CtrlRParen<'a>,
        ),
        Parened(CtrlLParen<'a>, Box<Self>, CtrlRParen<'a>),
    }

    #[derive(Debug)]
    pub enum ArrayedExprs<'a> {
        Expr(Expr<'a>),
        Braces(CtrlLBrace<'a>, SepList1<Self, CtrlComma<'a>>, CtrlRBrace<'a>),
        Hashes(SepList1<Self, CtrlHash<'a>>),
    }
    pub type ExprRange<'a> = (Expr<'a>, Option<(CtrlDotDot<'a>, Expr<'a>)>);
    #[derive(Debug)]
    pub enum ExprOrStr<'a> {
        Expr(Expr<'a>),
        Str(StrTok<'a>),
    }

    #[derive(Debug)]
    pub enum PrsExpr<'a> {
        Ident(Ident<'a>),
        Parened(CtrlLParen<'a>, Box<Self>, CtrlRParen<'a>),
        And(Ctrl<'a>, Box<Self>, Box<Self>),
        Or(Ctrl<'a>, Box<Self>, Box<Self>),
        Not(Ctrl<'a>, Box<Self>),
    }

    #[derive(Debug)]
    pub enum ArrayedExprIds<'a> {
        ExprId(ExprId<'a>),
        Braces(CtrlLBrace<'a>, SepList1<Self, CtrlComma<'a>>, CtrlRBrace<'a>),
        Hashes(SepList1<Self, CtrlHash<'a>>),
    }
    #[derive(Debug)]
    pub struct BaseId<'a> {
        pub ident: Ident<'a>,
        pub brackets: Vec<(CtrlRBracket<'a>, ExprRange<'a>, CtrlLBracket<'a>)>,
    }
    #[derive(Debug)]
    pub struct ExprId<'a>(pub SepList1<BaseId<'a>, CtrlDot<'a>>);

    #[derive(Debug)]
    pub struct SupplySpec<'a>(
        pub CtrlLAngBrace<'a>,
        pub ExprId<'a>,
        pub Option<(CtrlComma<'a>, ExprId<'a>)>,
        pub Option<(CtrlVBar<'a>, ExprId<'a>, CtrlComma<'a>, ExprId<'a>)>,
        pub CtrlRAngBrace<'a>,
    );
}
use ast::*;
// supply_spec: "<" expr_id [ "," expr_id ] [ "|" expr_id "," expr_id ] ">"

#[inline]
pub fn num<'a>(i: &'a [Tok]) -> IResult<&'a [Tok], Num<'a>, ET<'a>> {
    tag(&[TokenKind::Num as u8][..])
        .map(|vs: &'a [Tok]| &vs[0])
        .map(Num)
        .parse(i)
}
#[inline]
pub fn ident<'a>(i: &'a [Tok]) -> IResult<&'a [Tok], Ident<'a>, ET<'a>> {
    tag(&[TokenKind::Ident as u8][..])
        .map(|vs: &'a [Tok]| &vs[0])
        .map(Ident)
        .parse(i)
}
#[inline]
pub fn string<'a>(i: &'a [Tok]) -> IResult<&'a [Tok], StrTok<'a>, ET<'a>> {
    tag(&[TokenKind::Str as u8][..])
        .map(|vs: &'a [Tok]| &vs[0])
        .map(StrTok)
        .parse(i)
}

// TODO maybe make a struct for this (like CtrlC)
#[inline]
pub fn kw<'a>(s: &str) -> KwC {
    macro_rules! make_case {
        ($tok:expr) => {{
            const ARR: [u8; 1] = [$tok as u8];
            const LABEL: &'static str = stringify!($c);
            KwC {
                buf: &ARR,
                label: LABEL,
            }
        }};
    }

    match s {
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
    }
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

pub fn qualified_name(i: &[u8]) -> IResult<&[u8], QualifiedName, ET> {
    let leading_colons = ctrl2(':', ':').opt();
    leading_colons
        .then(ident.list1_sep_by(ctrl2(':', ':')).p())
        .map(|(a, b)| QualifiedName(a, b))
        .context("qualified name")
        .parse(i)
}

// attr_list: "[" { ID "=" expr ";" }** "]"
pub fn attr_list(i: &[u8]) -> IResult<&[u8], BracketedAttrList, ET> {
    let one_attr = ident.then(ctrl('=')).then(expr).map(|((a, b), c)| (a, b, c));
    one_attr
        .list1_sep_by(ctrl(';'))
        .bracketed()
        .map(|(a, b, c)| BracketedAttrList(a, b, c))
        .parse(i)
}

pub fn dir(i: &[u8]) -> IResult<&[u8], (Dir, Ctrl), ET> {
    alt((ctrl('+').map(|v| (Dir::Plus, v)), ctrl('-').map(|v| (Dir::Minus, v)))).parse(i)
}

fn unary_rec<'a, F>(expr: F) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, ET<'a>>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, ET<'a>>,
{
    move |i| {
        // 'Atoms' are expressions that contain no ambiguity
        let concat = ctrl('{')
            .then_cut((&expr).list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| Expr::Concat(a, b, c));

        let bitfield = ident
            .then(ctrl('{'))
            .then_cut(
                (&expr)
                    .then_opt(peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then((&expr).map(Box::new))))
                    .then(ctrl('}')),
            )
            .map(|((a, b), ((c, d), e))| Expr::BitField(a, b, Box::new(c), d, e));

        let func_name = alt((
            kw("int").map(FuncName::Int),
            kw("bool").map(FuncName::Bool),
            ident.map(FuncName::Ident),
        ));
        let func_call = func_name
            .then(ctrl('('))
            .then_cut((&expr).list1_sep_by(ctrl(',')).term_by(ctrl(')')))
            .map(|((name, lparen), (args, rparen))| Expr::Call(name, lparen, args, rparen));

        let atom = alt((
            num.map(Expr::Num),
            // bitfield and func_call go before "local"
            bitfield,
            func_call,
            ident.map(Expr::Ident), // TODO support qualified names?
            ctrl('(')
                .then_cut(&expr)
                .then(ctrl(')'))
                .map(|((a, b), c)| Expr::Parened(a, Box::new(b), c)),
            concat,
        ));

        // https://en.cppreference.com/w/c/language/operator_precedence

        // apply function calls, array accesses, and dot operators
        enum AccessKind<'a> {
            // Func(Vec<Spanned<Expr>>),
            Arr(
                CtrlLBracket<'a>,
                Expr<'a>,
                Option<(CtrlDotDot<'a>, Box<Expr<'a>>)>,
                CtrlRBracket<'a>,
            ),
            Dot(CtrlDot<'a>, Ident<'a>),
        }

        let arr_access = ctrl('[')
            .then_cut(
                (&expr)
                    .then_opt(peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then((&expr).map(Box::new))))
                    .then(ctrl(']')),
            )
            .map(|(a, ((b, c), d))| AccessKind::Arr(a, b, c, d));
        let dot_access =
            peek(not(ctrl2('.', '.'))).ignore_then(ctrl('.').then_cut(ident).map(|(a, b)| AccessKind::Dot(a, b)));

        let access = atom
            .then(
                arr_access
                    .or(dot_access)
                    .many0()
                    .term_by_peek_not_alt2(ctrl('['), ctrl('.')),
            )
            .map(|(lhs, accs)| {
                accs.into_iter().fold(lhs, |lhs, access| match access {
                    AccessKind::Arr(a, b, c, d) => Expr::ArrAccess(Box::new(lhs), a, Box::new(b), c, d),
                    AccessKind::Dot(a, b) => Expr::Dot(Box::new(lhs), a, b),
                })
            });

        // Unary operators have precidence 2
        let op = alt((
            ctrl('-').map(|v| (UnaryOp::UMinus, v)),
            ctrl('~').map(|v| (UnaryOp::Not, v)),
        ));

        // This is an ok use of the nom version of "many0", because all of the things being parsed are 1 token long
        nom::multi::many0(op)
            .then(access)
            .map(|(ops, a)| ops.iter().rev().fold(a, |e, (op, c)| Expr::Unary(*op, *c, Box::new(e))))
            .parse(i)
    }
}

fn binary_rec<'a, F, G>(expr: F, infix_binary_ops: G) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, ET<'a>>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, ET<'a>>,
    G: Fn(&'a [u8]) -> IResult<&'a [u8], (BinaryOp, Ctrl<'a>), ET<'a>>,
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
                let fold_nodes_with_ops = |node0, nodes: Vec<((BinaryOp, Ctrl<'a>), Expr<'a>)>, ops: Vec<BinaryOp>| {
                    const DUMMY_TOK: [u8; 1] = [0u8];
                    let dummy_ctrl = Ctrl::Ctrl(&DUMMY_TOK[0]);
                    let mut result = nodes.into_iter().fold(
                        vec![((BinaryOp::Mul /*dummy value*/, dummy_ctrl), node0)],
                        |mut ls, ((r_op, c), r)| {
                            if ops.contains(&r_op) {
                                let ((l_op, cl), l) = ls.pop().unwrap();
                                let new_node: Expr = Expr::Binary(r_op, c, Box::new(l), Box::new(r));
                                ls.push(((l_op, cl), new_node));
                            } else {
                                ls.push(((r_op, c), r));
                            }
                            ls
                        },
                    );
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

fn query_rec<'a, F, G>(expr: F, infix_binary_ops: G) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, ET<'a>>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr<'a>, ET<'a>>,
    G: Fn(&'a [u8]) -> IResult<&'a [u8], (BinaryOp, Ctrl<'a>), ET<'a>>,
{
    move |i| {
        // handles `a ? b : c ? d : e` as `a ? b : (c ? d : e)`
        binary_rec(&expr, &infix_binary_ops)
            .then(
                (ctrl('?')
                    .then(binary_rec(&expr, &infix_binary_ops))
                    .then(ctrl(':'))
                    .then(binary_rec(&expr, &infix_binary_ops)))
                .many0()
                .term_by_peek_not(ctrl('?')),
            )
            .map(|(sel0, nodes)| {
                let (v1s, mut sels): (Vec<((Ctrl<'a> /* ? */, Expr<'a>), Ctrl<'a> /* : */)>, Vec<Expr<'a>>) =
                    nodes.into_iter().unzip();
                if sels.len() == 0 {
                    sel0
                } else {
                    let final_v2 = sels.pop().unwrap();
                    sels.insert(0usize, sel0);

                    assert_eq!(sels.len(), v1s.len());
                    sels.into_iter()
                        .rev()
                        .zip(v1s.into_iter().rev())
                        .fold(final_v2, |v2, (sel, ((cq, v1), cc))| {
                            Expr::Query(Box::new(sel), cq, Box::new(v1), cc, Box::new(v2))
                        })
                }
            })
            .parse(i)
    }
}

fn infix_binary_ops(i: &[u8]) -> IResult<&[u8], (BinaryOp, Ctrl), ET> {
    alt((
        ctrl('*').map(|v| (BinaryOp::Mul, v)),
        ctrl('/').map(|v| (BinaryOp::Div, v)),
        ctrl('%').map(|v| (BinaryOp::Div, v)),
        ctrl('+').map(|v| (BinaryOp::Plus, v)),
        // This would be invalid syntax anyway, and this stops us from parsing a-> b as a misformatted expression
        ctrl('-')
            .then_ignore(peek(not(ctrl('>'))))
            .map(|v| (BinaryOp::Minus, v)),
        ctrl2('>', '>').map(|v| (BinaryOp::RShift, v)),
        ctrl2('<', '<').map(|v| (BinaryOp::LShift, v)),
        ctrl3('>', '>', '>').map(|v| (BinaryOp::ARShift, v)),
        ctrl('>').map(|v| (BinaryOp::Gt, v)),
        ctrl('<').map(|v| (BinaryOp::Lt, v)),
        ctrl2('>', '=').map(|v| (BinaryOp::Geq, v)),
        ctrl2('<', '=').map(|v| (BinaryOp::Leq, v)),
        ctrl('=').map(|v| (BinaryOp::Eq, v)),
        ctrl2('!', '=').map(|v| (BinaryOp::NotEq, v)),
        ctrl('&').map(|v| (BinaryOp::And, v)),
        ctrl('^').map(|v| (BinaryOp::Xor, v)),
        ctrl('|').map(|v| (BinaryOp::Or, v)),
    ))(i)
}

fn infix_binary_ops_no_gt(i: &[u8]) -> IResult<&[u8], (BinaryOp, Ctrl), ET> {
    alt((
        ctrl('*').map(|v| (BinaryOp::Mul, v)),
        ctrl('/').map(|v| (BinaryOp::Div, v)),
        ctrl('%').map(|v| (BinaryOp::Div, v)),
        ctrl('+').map(|v| (BinaryOp::Plus, v)),
        // This would be invalid syntax anyway, and this stops us from parsing a-> b as a misformatted expression
        ctrl('-')
            .then_ignore(peek(not(ctrl('>'))))
            .map(|v| (BinaryOp::Minus, v)),
        // ctrl2('>', '>').map(|v| (BinaryOp::RShift, v)),
        ctrl2('<', '<').map(|v| (BinaryOp::LShift, v)),
        // ctrl3('>', '>', '>').map(|v| (BinaryOp::ARShift, v)),
        // ctrl('>').map(|v| (BinaryOp::Gt, v)),
        ctrl('<').map(|v| (BinaryOp::Lt, v)),
        ctrl2('>', '=').map(|v| (BinaryOp::Geq, v)),
        ctrl2('<', '=').map(|v| (BinaryOp::Leq, v)),
        ctrl('=').map(|v| (BinaryOp::Eq, v)),
        ctrl2('!', '=').map(|v| (BinaryOp::NotEq, v)),
        ctrl('&').map(|v| (BinaryOp::And, v)),
        ctrl('^').map(|v| (BinaryOp::Xor, v)),
        ctrl('|').map(|v| (BinaryOp::Or, v)),
    ))(i)
}

pub fn expr(i: &[u8]) -> IResult<&[u8], Expr, ET> {
    query_rec(expr, infix_binary_ops).context("expr").parse(i)
}

pub fn expr_no_gt(i: &[u8]) -> IResult<&[u8], Expr, ET> {
    query_rec(expr, infix_binary_ops_no_gt).context("expr no gt").parse(i)
}

// arrayed_exprs: { array_term "#" }*
// array_term: {excl}
//            "{" { arrayed_exprs "," }* "}"
//           | expr
pub fn arrayed_exprs_no_gt(i: &[u8]) -> IResult<&[u8], ArrayedExprs, ET> {
    let term = alt((
        ctrl('{')
            .then_cut(arrayed_exprs.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| ArrayedExprs::Braces(a, b, c)),
        expr_no_gt.map(ArrayedExprs::Expr),
    ));

    term.list1_sep_by(ctrl('#'))
        .p()
        .map(ArrayedExprs::Hashes)
        .context("arrayed exprs no gt")
        .parse(i)
}
pub fn arrayed_exprs<'a>(i: &'a [u8]) -> IResult<&'a [u8], ArrayedExprs<'a>, ET<'a>> {
    let term = alt((
        ctrl('{')
            .then_cut(arrayed_exprs.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| ArrayedExprs::Braces(a, b, c)),
        expr.map(ArrayedExprs::Expr),
    ));

    term.list1_sep_by(ctrl('#'))
        .p()
        .map(ArrayedExprs::Hashes)
        .context("arrayed exprs")
        .parse(i)
}

pub fn expr_range(i: &[u8]) -> IResult<&[u8], ExprRange, ET> {
    let dotted_part = peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then(expr));
    expr.then(dotted_part.opt()).context("expr range").parse(i)
}

pub fn expr_or_str(i: &[u8]) -> IResult<&[u8], ExprOrStr, ET> {
    expr.map(ExprOrStr::Expr).or(string.map(ExprOrStr::Str)).parse(i)
}

fn prs_unary_rec(i: &[u8]) -> IResult<&[u8], PrsExpr, ET> {
    // 'Atoms' are expressions that contain no ambiguity
    let atom = alt((
        ident.map(PrsExpr::Ident), // TODO support qualified names?
        ctrl('(')
            .then_cut(prs_expr.then(ctrl(')')))
            .map(|(a, (b, c))| PrsExpr::Parened(a, Box::new(b), c)),
    ));

    // https://en.cppreference.com/w/c/language/operator_precedence

    // Unary operators have precidence 2

    // This is an ok use of the nom version of "many0", because all of the things being parsed are 1 token long
    nom::multi::many0(ctrl('~'))
        .then(atom)
        .map(|(ops, a)| ops.iter().rev().fold(a, |e, c| PrsExpr::Not(*c, Box::new(e))))
        .parse(i)
}

pub fn prs_expr(i: &[u8]) -> IResult<&[u8], PrsExpr, ET> {
    // In order to make it compile in a reasonable amount of time, we do binary operator parsing in two steps.
    // First we parse a chain of binary operators, and then we parse the precidence within the chains

    let op = alt((ctrl('&').map(|v| (true, v)), ctrl('|').map(|v| (true, v))));
    prs_unary_rec
        .then(
            op.then(prs_unary_rec)
                .many0()
                .term_by_peek_not_alt2(ctrl('&'), ctrl('|')),
        )
        .map(|(n0, ns): (PrsExpr, Vec<((bool, Ctrl), PrsExpr)>)| {
            // this bit of code is gross as it uses a mutable algorithm. It scans over the list [node0, nodes[0], nodes[1], ...]
            // and combines any pair of nodes with one of the specified operations in between
            // AND binds tigheter than OR
            let (acc_with_or_ctrl, anded_acc) = ns.into_iter().fold(
                (None, n0),
                |(acc_with_or_ctrl, anded_acc), ((is_and, c), r)| match is_and {
                    true => (acc_with_or_ctrl, PrsExpr::And(c, Box::new(anded_acc), Box::new(r))),
                    false => match acc_with_or_ctrl {
                        Some((acc, or_ctrl)) => {
                            (Some((PrsExpr::Or(or_ctrl, Box::new(acc), Box::new(anded_acc)), c)), r)
                        }
                        None => (Some((anded_acc, c)), r),
                    },
                },
            );
            let acc = match acc_with_or_ctrl {
                Some((acc, or_ctrl)) => PrsExpr::Or(or_ctrl, Box::new(acc), Box::new(anded_acc)),
                None => anded_acc,
            };
            acc
        })
        .parse(i)
}

pub fn arrayed_expr_ids(i: &[u8]) -> IResult<&[u8], ArrayedExprIds, ET> {
    // arrayed_expr_ids: { lhs_array_term "#" }*
    // lhs_array_term: {excl}
    //                "{" { arrayed_expr_ids "," }* "}"
    //               | expr_id
    let term = alt((
        ctrl('{')
            .then_cut(arrayed_expr_ids.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| ArrayedExprIds::Braces(a, b, c)),
        expr_id.map(ArrayedExprIds::ExprId),
    ));

    term.list1_sep_by(ctrl('#'))
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

pub fn base_id(i: &[u8]) -> IResult<&[u8], BaseId, ET> {
    let bracketed_spare_ranges = expr_range.bracketed().many0().term_by_peek_not(ctrl('['));

    // first look ahead to see if there are brackets. If there are, then parse brackets until there arent brackets any more
    ident
        .then(bracketed_spare_ranges)
        .map(|(ident, brackets)| BaseId { ident, brackets })
        .parse(i)
}

pub fn expr_id(i: &[u8]) -> IResult<&[u8], ExprId, ET> {
    base_id
        .list1_sep_by(ctrl('.'))
        .p()
        .map(ExprId)
        .context("expr id")
        .parse(i)
}

pub fn supply_spec(i: &[u8]) -> IResult<&[u8], SupplySpec, ET> {
    expr_id
        .then_opt(ctrl(',').then_cut(expr_id))
        .then_opt(
            ctrl('|')
                .then_cut(expr_id.then(ctrl(',')).then(expr_id))
                .map(|(a, ((b, c), d))| (a, b, c, d)),
        )
        .ang_braced()
        .map(|(a, ((b, c), d), e)| SupplySpec(a, b, c, d, e))
        .context("supply spec")
        .parse(i)
}
