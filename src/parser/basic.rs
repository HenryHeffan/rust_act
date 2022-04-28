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
    pub struct FTPtr {
        ptr: usize,
    }
    #[derive(Debug, Copy, Clone)]
    pub struct FTStart(FTPtr);
    impl FTPtr {
        pub fn of_ptr(ft: &u8) -> FTPtr {
            FTPtr {
                ptr: ft as *const u8 as usize,
            }
        }
        pub fn dummy() -> FTPtr {
            FTPtr { ptr: 0 }
        }
        pub fn idx(&self, ft_start: FTStart) -> usize {
            self.ptr - ft_start.0.ptr
        }
    }
    impl FTStart {
        pub fn of_vec(fts: &Vec<FlatToken>) -> FTStart {
            match fts.first() {
                None => FTStart(FTPtr::dummy()), // because there is noting in the array
                Some(v) => FTStart(FTPtr::of_ptr(v)),
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    pub struct Num(pub FTPtr);
    #[derive(Debug, Copy, Clone)]
    pub struct Ident(pub FTPtr);
    #[derive(Debug, Copy, Clone)]
    pub struct StrTok(pub FTPtr);
    #[derive(Debug, Copy, Clone)]
    pub struct Kw(pub FTPtr);

    #[derive(Debug, Copy, Clone)]
    pub enum Ctrl {
        Ctrl(FTPtr),
        Ctrl2(FTPtr, FTPtr),
        Ctrl3(FTPtr, FTPtr, FTPtr),
    }
    pub type CtrlEquals = Ctrl;
    pub type CtrlLParen = Ctrl;
    pub type CtrlRParen = Ctrl;
    pub type CtrlLBracket = Ctrl;
    pub type CtrlRBracket = Ctrl;
    pub type CtrlLBrace = Ctrl;
    pub type CtrlRBrace = Ctrl;
    pub type CtrlLAngBrace = Ctrl;
    pub type CtrlRAngBrace = Ctrl;
    pub type CtrlDot = Ctrl;
    pub type CtrlDotDot = Ctrl;
    pub type CtrlColon = Ctrl;
    pub type CtrlColonColon = Ctrl;
    pub type CtrlComma = Ctrl;
    pub type CtrlSemi = Ctrl;
    pub type CtrlHash = Ctrl; // #
    pub type CtrlLArrow = Ctrl; // ->
    pub type CtrlAtSign = Ctrl;
    pub type CtrlVBar = Ctrl; // |
    pub type CtrlQMark = Ctrl;
    pub type CtrlStar = Ctrl; // *

    #[derive(Debug)]
    pub struct QualifiedName(pub Option<CtrlColonColon>, pub SepList1<Ident, CtrlColonColon>);

    #[derive(Debug)]
    pub struct BracketedAttrList(
        pub CtrlLBracket,
        pub SepList1<(Ident, CtrlEquals, Expr), CtrlSemi>,
        pub CtrlRBracket,
    );

    #[derive(Debug)]
    pub enum Arrayed<T> {
        Base(T),
        Braces(CtrlLBrace, SepList1<Self, CtrlComma>, CtrlRBrace),
        Hashes(SepList1<Self, CtrlHash>),
    }

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
    pub enum FuncName {
        Ident(Ident),
        Int(Kw),
        Bool(Kw),
    }

    // An expression node in the AST. Children are spanned so we can generate useful runtime errors.
    #[derive(Debug, Clone)]
    pub enum Expr {
        Num(Num),
        Ident(Ident),
        // meaning a variable or a channel that is an input to the expressions
        Unary(UnaryOp, Ctrl, Box<Self>),
        Binary(BinaryOp, Ctrl, Box<Self>, Box<Self>),
        BitField(
            Ident,
            CtrlLBrace,
            Box<Self>,
            Option<(CtrlDotDot, Box<Self>)>,
            CtrlRBrace,
        ),
        ArrAccess(
            Box<Self>,
            CtrlLBracket,
            Box<Self>,
            Option<(CtrlDotDot, Box<Self>)>,
            CtrlRBracket,
        ),
        Query(Box<Self>, CtrlQMark, Box<Self>, CtrlColon, Box<Self>),
        Concat(CtrlLBrace, SepList1<Self, CtrlComma>, CtrlRBrace),
        Dot(Box<Self>, CtrlDot, Ident),
        Call(FuncName, CtrlLParen, SepList1<Self, CtrlComma>, CtrlRParen),
        Parened(CtrlLParen, Box<Self>, CtrlRParen),
    }
    pub type ArrayedExprs = Arrayed<Expr>;

    pub type ExprRange = (Expr, Option<(CtrlDotDot, Expr)>);
    #[derive(Debug)]
    pub enum ExprOrStr {
        Expr(Expr),
        Str(StrTok),
    }

    #[derive(Debug)]
    pub enum PrsExpr {
        Ident(Ident),
        Parened(CtrlLParen, Box<Self>, CtrlRParen),
        And(Ctrl, Box<Self>, Box<Self>),
        Or(Ctrl, Box<Self>, Box<Self>),
        Not(Ctrl, Box<Self>),
        ArrAccess(Box<Self>, CtrlLBracket, ExprRange, CtrlRBracket),
        Dot(Box<Self>, CtrlDot, Ident),
    }

    #[derive(Debug)]
    pub struct BaseId {
        pub ident: Ident,
        pub brackets: Vec<(CtrlRBracket, ExprRange, CtrlLBracket)>,
    }
    #[derive(Debug)]
    pub struct ExprId(pub SepList1<BaseId, CtrlDot>);

    pub type ArrayedExprIds = Arrayed<ExprId>;

    #[derive(Debug)]
    pub struct SupplySpec(
        pub CtrlLAngBrace,
        pub ExprId,
        pub Option<(CtrlComma, ExprId)>,
        pub Option<(CtrlVBar, ExprId, CtrlComma, ExprId)>,
        pub CtrlRAngBrace,
    );
}
use ast::*;
// supply_spec: "<" expr_id [ "," expr_id ] [ "|" expr_id "," expr_id ] ">"

#[inline]
pub fn num<'a>(i: &'a [Tok]) -> IResult<&'a [Tok], Num, ET> {
    tag(&[TokenKind::Num as u8][..])
        .map(|vs: &'a [Tok]| Num(FTPtr::of_ptr(&vs[0])))
        .parse(i)
}
#[inline]
pub fn ident<'a>(i: &'a [Tok]) -> IResult<&'a [Tok], Ident, ET> {
    tag(&[TokenKind::Ident as u8][..])
        .map(|vs: &'a [Tok]| Ident(FTPtr::of_ptr(&vs[0])))
        .parse(i)
}
#[inline]
pub fn string<'a>(i: &'a [Tok]) -> IResult<&'a [Tok], StrTok, ET> {
    tag(&[TokenKind::Str as u8][..])
        .map(|vs: &'a [Tok]| StrTok(FTPtr::of_ptr(&vs[0])))
        .parse(i)
}

// TODO maybe make a struct for this (like CtrlC)
#[inline]
pub fn kw(s: &str) -> KwC {
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
pub fn ctrl_dot_not_dotdot() -> CtrlC {
    const PADDING: u8 = 0;
    const DUMMY: [u8; 3] = [PADDING, PADDING, PADDING];
    const LABEL: &'static str = ".";
    CtrlC {
        v: CtrlN::DotNotDotDot,
        spaced: &DUMMY,
        unspaced: &DUMMY,
        label: LABEL,
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

fn unary_rec<'a, F>(expr: F) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr, ET>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr, ET>,
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
        enum AccessKind {
            // Func(Vec<Spanned<Expr>>),
            Arr(CtrlLBracket, Expr, Option<(CtrlDotDot, Box<Expr>)>, CtrlRBracket),
            Dot(CtrlDot, Ident),
        }

        let arr_access = ctrl('[')
            .then_cut(
                (&expr)
                    .then_opt(peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then((&expr).map(Box::new))))
                    .then(ctrl(']')),
            )
            .map(|(a, ((b, c), d))| AccessKind::Arr(a, b, c, d));
        let dot_access = ctrl_dot_not_dotdot()
            .then_cut(ident)
            .map(|(a, b)| AccessKind::Dot(a, b));

        let access = atom
            .then(
                arr_access
                    .or(dot_access)
                    .many0()
                    .term_by_peek_not_alt2(ctrl('['), ctrl_dot_not_dotdot()),
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

fn binary_rec<'a, F, G>(expr: F, infix_binary_ops: G) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr, ET>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr, ET>,
    G: Fn(&'a [u8]) -> IResult<&'a [u8], (BinaryOp, Ctrl), ET>,
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
                let fold_nodes_with_ops = |node0, nodes: Vec<((BinaryOp, Ctrl), Expr)>, ops: Vec<BinaryOp>| {
                    let dummy_ctrl = Ctrl::Ctrl(FTPtr::dummy());
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

fn query_rec<'a, F, G>(expr: F, infix_binary_ops: G) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Expr, ET>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], Expr, ET>,
    G: Fn(&'a [u8]) -> IResult<&'a [u8], (BinaryOp, Ctrl), ET>,
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
                let (v1s, mut sels): (Vec<((Ctrl /* ? */, Expr), Ctrl /* : */)>, Vec<Expr>) = nodes.into_iter().unzip();
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
        ctrl3('>', '>', '>').map(|v| (BinaryOp::ARShift, v)),
        ctrl2('>', '>').map(|v| (BinaryOp::RShift, v)),
        ctrl2('<', '<').map(|v| (BinaryOp::LShift, v)),
        ctrl2('>', '=').map(|v| (BinaryOp::Geq, v)),
        ctrl2('<', '=').map(|v| (BinaryOp::Leq, v)),
        ctrl('>').map(|v| (BinaryOp::Gt, v)),
        ctrl('<').map(|v| (BinaryOp::Lt, v)),
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
        // ctrl3('>', '>', '>').map(|v| (BinaryOp::ARShift, v)),
        // ctrl2('>', '>').map(|v| (BinaryOp::RShift, v)),
        ctrl2('<', '<').map(|v| (BinaryOp::LShift, v)),
        ctrl2('>', '=').map(|v| (BinaryOp::Geq, v)),
        ctrl2('<', '=').map(|v| (BinaryOp::Leq, v)),
        // ctrl('>').map(|v| (BinaryOp::Gt, v)),
        ctrl('<').map(|v| (BinaryOp::Lt, v)),
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
pub fn arrayed_exprs_no_gt(i: &[u8]) -> IResult<&[u8], Arrayed<Expr>, ET> {
    let term = alt((
        ctrl('{')
            .then_cut(arrayed_exprs.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| Arrayed::Braces(a, b, c)),
        expr_no_gt.map(Arrayed::Base),
    ));

    term.list1_sep_by(ctrl('#'))
        .p()
        .map(Arrayed::Hashes)
        .context("arrayed exprs no gt")
        .parse(i)
}
pub fn arrayed_exprs(i: &[u8]) -> IResult<&[u8], Arrayed<Expr>, ET> {
    let term = alt((
        ctrl('{')
            .then_cut(arrayed_exprs.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| Arrayed::Braces(a, b, c)),
        expr.map(Arrayed::Base),
    ));

    term.list1_sep_by(ctrl('#'))
        .p()
        .map(Arrayed::Hashes)
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
    // apply function calls, array accesses, and dot operators
    enum AccessKind {
        // Func(Vec<Spanned<Expr>>),
        Arr(CtrlLBracket, ExprRange, CtrlRBracket),
        Dot(CtrlDot, Ident),
    }

    let arr_access = ctrl('[')
        .then_cut(expr_range.then(ctrl(']')))
        .map(|(a, (b, c))| AccessKind::Arr(a, b, c));
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
                AccessKind::Arr(a, b, c) => PrsExpr::ArrAccess(Box::new(lhs), a, b, c),
                AccessKind::Dot(a, b) => PrsExpr::Dot(Box::new(lhs), a, b),
            })
        });

    // This is an ok use of the nom version of "many0", because all of the things being parsed are 1 token long
    nom::multi::many0(ctrl('~'))
        .then(access)
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

pub fn arrayed_expr_ids(i: &[u8]) -> IResult<&[u8], Arrayed<ExprId>, ET> {
    // arrayed_expr_ids: { lhs_array_term "#" }*
    // lhs_array_term: {excl}
    //                "{" { arrayed_expr_ids "," }* "}"
    //               | expr_id
    let term = alt((
        ctrl('{')
            .then_cut(arrayed_expr_ids.list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| Arrayed::Braces(a, b, c)),
        expr_id.map(Arrayed::Base),
    ));

    term.list1_sep_by(ctrl('#'))
        .p()
        .map(Arrayed::Hashes)
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
        .list1_sep_by(ctrl_dot_not_dotdot())
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
