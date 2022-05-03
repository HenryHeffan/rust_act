use nom::{branch::alt, bytes::complete::tag, combinator::peek, IResult, Parser, sequence::tuple};
use nom_supreme::parser_ext::ParserExt;

use ast::*;

use crate::token::{FlatToken, TokenKind, Unspaced};
pub use crate::utils::{CtrlC, CtrlN, ET, KwC, Many0, Many1, MyParserExt, ParserExt2, uncut, Unterm};

pub mod ast {
    use crate::utils::SepList1;

    use super::*;

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
        pub fn offset_between(begin: &FTPtr, end: &FTPtr) -> usize {
            end.ptr - begin.ptr
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

    impl Ctrl {
        pub fn ft_ptr_last(&self) -> FTPtr {
            match self {
                Ctrl::Ctrl(c) |
                Ctrl::Ctrl2(_, c) |
                Ctrl::Ctrl3(_, _, c) => *c
            }
        }
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
    pub type CtrlHash = Ctrl;
    // #
    pub type CtrlLArrow = Ctrl;
    // ->
    pub type CtrlAtSign = Ctrl;
    pub type CtrlVBar = Ctrl;
    // |
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
        Tilde,
        Hash,
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

    #[derive(Debug)]
    pub enum FuncName {
        QualifiedName(QualifiedName),
        Int(Kw),
        Bool(Kw),
    }

    #[derive(Debug)]
    pub struct FuncTemplateParams(pub CtrlLAngBrace, pub SepList1<Expr, CtrlComma>, pub CtrlRAngBrace);

    // An expression node in the AST. Children are spanned so we can generate useful runtime errors.
    #[derive(Debug)]
    pub enum Expr {
        Num(Num),
        Ident(Ident),
        // meaning a variable or a channel that is an input to the expressions
        Unary(UnaryOp, Ctrl, Box<Self>),
        Binary(BinaryOp, Ctrl, Box<Self>, Box<Self>),
        BitField(
            Box<Self>,
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
        Call(
            FuncName,
            Option<FuncTemplateParams>,
            CtrlLParen,
            SepList1<Self, CtrlComma>,
            CtrlRParen,
        ),
        MacroLoop(
            CtrlLParen,
            (BinaryOp, Ctrl),
            Ident,
            CtrlColon,
            Box<ExprRange>,
            CtrlColon,
            Box<Self>,
            CtrlRParen,
        ),
        Parened(CtrlLParen, Box<Self>, CtrlRParen),
    }

    pub type ArrayedExprs = Arrayed<Expr>;

    pub type ExprRange = (Expr, Option<(CtrlDotDot, Expr)>);

    #[derive(Debug)]
    pub enum ExprOrStr {
        Expr(Expr),
        Str(StrTok),
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
    pub enum AndOrOr {
        And,
        Or,
    }

    #[derive(Debug)]
    pub enum PrsExpr {
        Num(Num),
        ExprId(ExprId),
        AtIdent(CtrlAtSign, Ident),
        Sized(
            Box<Self>,
            CtrlLAngBrace,
            SepList1<Expr, Ctrl /* I think ; or , */>,
            CtrlRAngBrace,
        ),
        Braced(CtrlLBrace, (Dir, Ctrl), Box<Self>, CtrlRBrace, Box<Self>),
        Parened(CtrlLParen, Box<Self>, CtrlRParen),
        MacroLoop(MacroLoop<(AndOrOr, Ctrl), Box<Self>>),
        And(Ctrl, Box<Self>, Box<Self>),
        Or(Ctrl, Box<Self>, Box<Self>),
        Not(Ctrl, Box<Self>),
    }

    #[derive(Debug)]
    pub struct BaseId {
        pub ident: Ident,
        pub brackets: Vec<(CtrlRBracket, ExprRange, CtrlLBracket)>,
    }

    #[derive(Debug)]
    pub struct ExprId(pub SepList1<BaseId, CtrlDot>);

    #[derive(Debug)]
    pub enum ExprIdOrStar {
        ExprId(ExprId),
        Star(CtrlStar),
    }

    #[derive(Debug)]
    pub enum ExprIdOrStarOrBar {
        ExprId(ExprId),
        Star(CtrlStar),
        Bar(CtrlVBar),
    }

    pub type ArrayedExprIds = Arrayed<ExprId>;

    #[derive(Debug)]
    pub struct SupplySpec(
        pub CtrlLAngBrace,
        pub ExprId,
        pub Option<(CtrlComma, ExprId)>,
        pub Option<(CtrlVBar, ExprId, CtrlComma, ExprId)>,
        pub CtrlRAngBrace,
    );

    #[derive(Debug)]
    pub struct MacroLoop<C, T>(
        pub CtrlLParen,
        pub C,
        pub Ident,
        pub CtrlColon,
        pub ExprRange,
        pub CtrlColon,
        pub T,
        pub CtrlRParen,
    );
}

#[inline]
pub fn num<'a, E: ET<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], Num, E> {
    tag(&[TokenKind::Num as u8][..])
        .map(|vs: &'a [Tok]| Num(FTPtr::of_ptr(&vs[0])))
        .parse(i)
}

#[inline]
pub fn ident<'a, E: ET<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], Ident, E> {
    tag(&[TokenKind::Ident as u8][..])
        .map(|vs: &'a [Tok]| Ident(FTPtr::of_ptr(&vs[0])))
        .parse(i)
}

#[inline]
pub fn string<'a, E: ET<'a>>(i: &'a [Tok]) -> IResult<&'a [Tok], StrTok, E> {
    tag(&[TokenKind::Str as u8][..])
        .map(|vs: &'a [Tok]| StrTok(FTPtr::of_ptr(&vs[0])))
        .parse(i)
}

// TODO maybe make a struct for this (like CtrlC)
#[inline(always)]
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
#[inline(always)]
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

#[inline(always)]
pub fn ctrl2_helper(c1: char, c2: char, ctrl1_not_ctrl2: bool) -> CtrlC {
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
                v: if ctrl1_not_ctrl2 {
                    CtrlN::Char1NotChar2
                } else {
                    CtrlN::Char2
                },
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

#[inline(always)]
pub fn ctrl1_not_ctrl2(c1: char, c2: char) -> CtrlC {
    ctrl2_helper(c1, c2, true)
}

#[inline(always)]
pub fn ctrl2(c1: char, c2: char) -> CtrlC {
    ctrl2_helper(c1, c2, false)
}

#[inline(always)]
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

#[inline(always)]
pub fn alt_ctrl11(c1: char, c2: char) -> CtrlC {
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
                TokenKind::from_ctrl($c2, Unspaced::No) as u8,
                PADDING,
            ];
    const LABEL: &'static str = stringify!($c1 $c2);
            CtrlC {
                v: CtrlN::Char1OrChar1,
                spaced: &SPACED,
                unspaced: &UNSPACED,
                label: LABEL,
            }
        }};
    }
    match (c1, c2) {
        (',', ';') => make_case!(',', ';'),
        c => panic!("no such parser implemented {:?}", c),
    }
}

pub fn qualified_name<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], QualifiedName, E> {
    ctrl2(':', ':').p().opt()
        .then(ident.list1_sep_by(ctrl2(':', ':')).p())
        .map(|(a, b)| QualifiedName(a, b))
        .context("qualified name")
        .parse(i)
}

// attr_list: "[" { ID "=" expr ";" }** "]"
pub fn cut_bracketed_attr_list<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], BracketedAttrList, E> {
    let one_attr = ident.then(ctrl('=')).then(expr).map(|((a, b), c)| (a, b, c));
    ctrl('[')
        .then_cut(one_attr.list1_sep_by(ctrl(';')).term_by(ctrl(']')))
        .map(|(a, (b, c))| BracketedAttrList(a, b, c))
        .parse(i)
}

pub fn dir<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], (Dir, Ctrl), E> {
    alt((ctrl('+').p().map(|v| (Dir::Plus, v)), ctrl('-').p().map(|v| (Dir::Minus, v)))).parse(i)
}

#[derive(Clone, Copy)]
struct UnaryExprRecParser<F: Clone + Copy> {
    expr: F,
}

#[inline]
fn expr_func_name<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], FuncName, E> {
    alt((
        kw("int").p().map(FuncName::Int),
        kw("bool").p().map(FuncName::Bool),
        uncut(qualified_name.map(FuncName::QualifiedName)),
    ))
        .parse(i)
}

impl<'a, F, E: ET<'a>> Parser<&'a [u8], Expr, E> for UnaryExprRecParser<F>
    where
        F: Parser<&'a [u8], Expr, E> + Clone + Copy,
{
    #[inline(never)]
    fn parse(&mut self, i: &'a [u8]) -> IResult<&'a [u8], Expr, E>
        where
            F: Parser<&'a [u8], Expr, E> + Clone + Copy,
    {
        // 'Atoms' are expressions that contain no ambiguity
        let concat = ctrl('{')
            .then_cut((&self.expr).list1_sep_by(ctrl(',')).term_by(ctrl('}')))
            .map(|(a, (b, c))| Expr::Concat(a, b, c));

        let func_call = expr_func_name
            .then(ctrl('(').then_cut((&self.expr).list1_sep_by(ctrl(',')).term_by(ctrl(')'))))
            .map(|(name, (lparen, (args, rparen)))| Expr::Call(name, None, lparen, args, rparen));
        let templated_func_call = expr_func_name
            .then_cut(
                ctrl('<')
                    .then(expr_no_gt.list1_sep_by(ctrl(',')).term_by(ctrl('>')))
                    .then(ctrl('('))
                    .then((&self.expr).list1_sep_by(ctrl(',')).term_by(ctrl(')'))),
            )
            .map(
                |(name, (((langle, (template_params, rangle)), lparen), (args, rparen)))| {
                    Expr::Call(
                        name,
                        Some(FuncTemplateParams(langle, template_params, rangle)),
                        lparen,
                        args,
                        rparen,
                    )
                },
            );

        let macro_loop_op = alt((
            ctrl('*').p().map(|v| (BinaryOp::Mul, v)),
            ctrl('+').p().map(|v| (BinaryOp::Plus, v)),
            ctrl('&').p().map(|v| (BinaryOp::And, v)),
            ctrl('^').p().map(|v| (BinaryOp::Xor, v)),
            ctrl('|').p().map(|v| (BinaryOp::Or, v)),
        ));

        let macro_loop = ctrl('(')
            .then(macro_loop_op)
            .then_cut(tuple((ident, ctrl(':'), expr_range, ctrl(':'), self.expr, ctrl(')'))))
            .context("prs expr macro loop")
            .map(|((a, b), (c, d, e, f, g, h))| Expr::MacroLoop(a, b, c, d, Box::new(e), f, Box::new(g), h));

        let atom = alt((
            num.map(Expr::Num).context("num"),
            // bitfield and func_call go before "local"
            func_call.context("func call"),                  // uncut
            uncut(templated_func_call.context("templated")), // this is ambiguous because it could also often be a expression
            ident.map(Expr::Ident).context("ident"),         // TODO support qualified names?
            macro_loop,
            ctrl('(')
                .then_cut((&self.expr).then(ctrl(')')))
                .map(|(a, (b, c))| Expr::Parened(a, Box::new(b), c))
                .context("parened expr"),
            concat.context("concat"),
        ))
            .context("atom");

        // https://en.cppreference.com/w/c/language/operator_precedence

        // apply function calls, array accesses, and dot operators
        enum AccessKind {
            // Func(Vec<Spanned<Expr>>),
            Arr(CtrlLBracket, Expr, Option<(CtrlDotDot, Box<Expr>)>, CtrlRBracket),
            Dot(CtrlDot, Ident),
        }

        let arr_access = ctrl1_not_ctrl2('[', ']')
            .then_cut(
                (&self.expr)
                    .then_opt(peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then((&self.expr).map(Box::new))))
                    .then(ctrl(']')),
            )
            .map(|(a, ((b, c), d))| AccessKind::Arr(a, b, c, d));
        let dot_access = ctrl1_not_ctrl2('.', '.')
            .then_cut(ident)
            .map(|(a, b)| AccessKind::Dot(a, b));

        let access = atom
            .then(
                arr_access
                    .or(dot_access)
                    .many0()
                    .term_by_peek_not_alt2(ctrl1_not_ctrl2('[', ']'), ctrl1_not_ctrl2('.', '.')),
            )
            .context("access")
            .map(|(lhs, accs)| {
                accs.into_iter().fold(lhs, |lhs, access| match access {
                    AccessKind::Arr(a, b, c, d) => Expr::ArrAccess(Box::new(lhs), a, Box::new(b), c, d),
                    AccessKind::Dot(a, b) => Expr::Dot(Box::new(lhs), a, b),
                })
            });

        let bitfield = access
            .then_opt(
                ctrl('{').then_cut(
                    (&self.expr)
                        .then_opt(peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then((&self.expr).map(Box::new))))
                        .then(ctrl('}')),
                ),
            )
            .map(|(a, o)| match o {
                None => a,
                Some((b, ((c, d), e))) => Expr::BitField(Box::new(a), b, Box::new(c), d, e),
            });

        // Unary operators have precidence 2
        let op = alt((
            ctrl('-').p().map(|v| (UnaryOp::UMinus, v)),
            ctrl('~').p().map(|v| (UnaryOp::Tilde, v)),
            ctrl('#').p().map(|v| (UnaryOp::Hash, v)),
        ));

        // This is an ok use of the nom version of "many0", because all of the things being parsed are 1 token long
        nom::multi::many0(op)
            .then(bitfield)
            .map(|(ops, a)| ops.iter().rev().fold(a, |e, (op, c)| Expr::Unary(*op, *c, Box::new(e))))
            .parse(i)
    }
}

#[derive(Clone, Copy)]
struct BinaryExprRecParser<F: Clone + Copy, G: Clone + Copy> {
    unary_expr: F,
    infix_binary_ops: G,
}

impl<'a, F, G, E: ET<'a>> Parser<&'a [u8], Expr, E> for BinaryExprRecParser<F, G>
    where
        F: Parser<&'a [u8], Expr, E> + Clone + Copy,
        G: Parser<&'a [u8], (BinaryOp, Ctrl), E> + Clone + Copy,
{
    #[inline(never)]
    fn parse(&mut self, i: &'a [u8]) -> IResult<&'a [u8], Expr, E> {
        // In order to make it compile in a reasonable amount of time, we do binary operator parsing in two steps.
        // First we parse a chain of binary operators, and then we parse the precidence within the chains
        (self.unary_expr)
            .then(
                (self.infix_binary_ops)
                    .then(self.unary_expr)
                    .many0()
                    .term_by_peek_not(self.infix_binary_ops),
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

struct QueryParser<F: Clone + Copy, G: Clone + Copy> {
    expr: F,
    infix_binary_ops: G,
}

impl<'a, F, G, E: ET<'a>> Parser<&'a [u8], Expr, E> for QueryParser<F, G>
    where
        F: Parser<&'a [u8], Expr, E> + Clone + Copy,
        G: Parser<&'a [u8], (BinaryOp, Ctrl), E> + Clone + Copy,
{
    #[inline(never)]
    fn parse(&mut self, i: &'a [u8]) -> IResult<&'a [u8], Expr, E> {
        let binary_op = BinaryExprRecParser {
            unary_expr: UnaryExprRecParser { expr: self.expr },
            infix_binary_ops: self.infix_binary_ops,
        };
        // handles `a ? b : c ? d : e` as `a ? b : (c ? d : e)`
        binary_op
            .then(
                (ctrl('?').then(|i| self.parse(i)).then(ctrl(':')).then(binary_op))
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

fn infix_binary_ops<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], (BinaryOp, Ctrl), E> {
    alt((
        ctrl('*').p().map(|v| (BinaryOp::Mul, v)),
        ctrl('/').p().map(|v| (BinaryOp::Div, v)),
        ctrl('%').p().map(|v| (BinaryOp::Div, v)),
        ctrl('+').p().map(|v| (BinaryOp::Plus, v)),
        // This would be invalid syntax anyway, and this stops us from parsing a-> b as a misformatted expression
        ctrl1_not_ctrl2('-', '>').p().map(|v| (BinaryOp::Minus, v)),
        ctrl3('>', '>', '>').p().map(|v| (BinaryOp::ARShift, v)),
        ctrl2('>', '>').p().map(|v| (BinaryOp::RShift, v)),
        ctrl2('<', '<').p().map(|v| (BinaryOp::LShift, v)),
        ctrl2('>', '=').p().map(|v| (BinaryOp::Geq, v)),
        ctrl2('<', '=').p().map(|v| (BinaryOp::Leq, v)),
        ctrl('>').p().map(|v| (BinaryOp::Gt, v)),
        ctrl('<').p().map(|v| (BinaryOp::Lt, v)),
        ctrl('=').p().map(|v| (BinaryOp::Eq, v)),
        ctrl2('!', '=').p().map(|v| (BinaryOp::NotEq, v)),
        ctrl('&').p().map(|v| (BinaryOp::And, v)),
        ctrl('^').p().map(|v| (BinaryOp::Xor, v)),
        ctrl1_not_ctrl2('|', ']').p().map(|v| (BinaryOp::Or, v)),
    ))(i)
}

fn infix_binary_ops_no_gt<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], (BinaryOp, Ctrl), E> {
    alt((
        ctrl('*').p().map(|v| (BinaryOp::Mul, v)),
        ctrl('/').p().map(|v| (BinaryOp::Div, v)),
        ctrl('%').p().map(|v| (BinaryOp::Div, v)),
        ctrl('+').p().map(|v| (BinaryOp::Plus, v)),
        // This would be invalid syntax anyway, and this stops us from parsing a-> b as a misformatted expression
        ctrl1_not_ctrl2('-', '>').p().map(|v| (BinaryOp::Minus, v)),
        // ctrl3('>', '>', '>').map(|v| (BinaryOp::ARShift, v)),
        // ctrl2('>', '>').map(|v| (BinaryOp::RShift, v)),
        ctrl2('<', '<').p().map(|v| (BinaryOp::LShift, v)),
        ctrl2('>', '=').p().map(|v| (BinaryOp::Geq, v)),
        ctrl2('<', '=').p().map(|v| (BinaryOp::Leq, v)),
        // ctrl('>').map(|v| (BinaryOp::Gt, v)),
        ctrl('<').p().map(|v| (BinaryOp::Lt, v)),
        ctrl('=').p().map(|v| (BinaryOp::Eq, v)),
        ctrl2('!', '=').p().map(|v| (BinaryOp::NotEq, v)),
        ctrl('&').p().map(|v| (BinaryOp::And, v)),
        ctrl('^').p().map(|v| (BinaryOp::Xor, v)),
        ctrl1_not_ctrl2('|', ']').p().map(|v| (BinaryOp::Or, v)),
    ))(i)
}

#[inline(never)]
pub fn expr<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Expr, E> {
    QueryParser { expr, infix_binary_ops }.context("expr").parse(i)
}

#[inline(never)]
pub fn expr_no_qmark<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Expr, E> {
    BinaryExprRecParser {
        unary_expr: UnaryExprRecParser { expr },
        infix_binary_ops,
    }
        .context("expr")
        .parse(i)
}

#[inline(never)]
pub fn expr_no_gt<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Expr, E> {
    QueryParser {
        expr,
        infix_binary_ops: infix_binary_ops_no_gt,
    }
        .context("expr no gt")
        .parse(i)
}

pub fn expr_range<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ExprRange, E> {
    let dotted_part = peek(ctrl('.')).ignore_then_cut(ctrl2('.', '.').then(expr));
    expr.then(dotted_part.opt()).context("expr range").parse(i)
}

pub fn expr_or_str<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ExprOrStr, E> {
    expr.map(ExprOrStr::Expr).or(string.map(ExprOrStr::Str)).parse(i)
}

fn prs_unary_rec<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], PrsExpr, E> {
    let and_or_or = alt((
        ctrl('&').p().map(|v| (AndOrOr::And, v)),
        ctrl('|').p().map(|v| (AndOrOr::Or, v)),
    ));

    // 'Atoms' are expressions that contain no ambiguity
    let atom = alt((
        num.map(PrsExpr::Num),
        ctrl('@').then(ident).map(|(a, b)| PrsExpr::AtIdent(a, b)),
        expr_id.map(PrsExpr::ExprId), // TODO support qualified names?
        ctrl('(')
            .then(and_or_or)
            .then_cut(tuple((ident, ctrl(':'), expr_range, ctrl(':'), prs_expr, ctrl(')'))))
            .context("prs expr macro loop")
            .map(|((a, b), (c, d, e, f, g, h))| MacroLoop(a, b, c, d, e, f, Box::new(g), h))
            .map(PrsExpr::MacroLoop),
        ctrl('(')
            .then_cut(prs_expr.then(ctrl(')')))
            .map(|(a, (b, c))| PrsExpr::Parened(a, Box::new(b), c)),
    ));

    // https://en.cppreference.com/w/c/language/operator_precedence

    // TODO I dont know if these are right!
    let sized = atom
        .then_opt(ctrl('<').then_cut(expr_no_gt.list1_sep_by(alt_ctrl11(',', ';')).term_by(ctrl('>'))))
        .map(|(lhs, sizing)| match sizing {
            None => lhs,
            Some((langle, (es, rangle))) => PrsExpr::Sized(Box::new(lhs), langle, es, rangle),
        });
    let braced_clause = ctrl('{').then_cut(dir.then(prs_expr).then(ctrl('}')));
    let braced = braced_clause.opt().then(sized).map(|(braces, lhs)| match braces {
        None => lhs,
        Some((lbrace, ((dir, o), rbrace))) => PrsExpr::Braced(lbrace, dir, Box::new(o), rbrace, Box::new(lhs)),
    });

    // This is an ok use of the nom version of "many0", because all of the things being parsed are 1 token long
    nom::multi::many0(ctrl('~'))
        .then(braced)
        .map(|(ops, a)| ops.iter().rev().fold(a, |e, c| PrsExpr::Not(*c, Box::new(e))))
        .parse(i)
}

pub fn prs_expr<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], PrsExpr, E> {
    // In order to make it compile in a reasonable amount of time, we do binary operator parsing in two steps.
    // First we parse a chain of binary operators, and then we parse the precidence within the chains

    let op = alt((ctrl('&').p().map(|v| (true, v)), ctrl('|').p().map(|v| (true, v))));
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

// arrayed_exprs: { array_term "#" }*
// array_term: {excl}
//            "{" { arrayed_exprs "," }* "}"
//           | expr
// arrayed_expr_ids: { lhs_array_term "#" }*
// lhs_array_term: {excl}
//                "{" { arrayed_expr_ids "," }* "}"
//               | expr_id
#[inline]
pub fn arrayed<'a, T, F, G, E: ET<'a>>(array: F, base: G) -> impl Parser<&'a [u8], Arrayed<T>, E>
    where
        F: Fn(&'a [u8]) -> IResult<&'a [u8], Arrayed<T>, E>,
        G: Fn(&'a [u8]) -> IResult<&'a [u8], T, E>,
{
    move |i| {
        let term = alt((
            ctrl('{')
                .then_cut((&array).list1_sep_by(ctrl(',')).term_by(ctrl('}')))
                .map(|(a, (b, c))| Arrayed::Braces(a, b, c)),
            (&base).map(Arrayed::Base),
        ));

        term.list1_sep_by(ctrl('#')).p().map(Arrayed::Hashes).parse(i)
    }
}

pub fn arrayed_exprs_no_gt<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Arrayed<Expr>, E> {
    arrayed(arrayed_exprs, expr_no_gt)
        .context("arrayed exprs no gt")
        .parse(i)
}

pub fn arrayed_exprs<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Arrayed<Expr>, E> {
    arrayed(arrayed_exprs, expr).context("arrayed exprs").parse(i)
}

pub fn arrayed_expr_ids<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Arrayed<ExprId>, E> {
    arrayed(arrayed_expr_ids, expr_id).context("arrayed expr ids").parse(i)
}

// xsparse_range: xsparse_one_range xsparse_range
//               | xsparse_one_range
// xsparse_one_range: "[" !noreal expr [ ".." expr ] "]"
// base_id: ID [ xsparse_range ]
// expr_id: { base_id "." }*

pub fn base_id<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], BaseId, E> {
    let bracketed_spare_ranges = expr_range
        .bracketed()
        .many0()
        .term_by_peek_not(ctrl1_not_ctrl2('[', ']'));

    // first look ahead to see if there are brackets. If there are, then parse brackets until there arent brackets any more
    ident
        .then(bracketed_spare_ranges)
        .map(|(ident, brackets)| BaseId { ident, brackets })
        .parse(i)
}

pub fn expr_id<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ExprId, E> {
    base_id
        .list1_sep_by(ctrl1_not_ctrl2('.', '.'))
        .p()
        .map(ExprId)
        .context("expr id")
        .parse(i)
}

pub fn expr_id_or_star<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ExprIdOrStar, E> {
    alt((
        ctrl('*').p().map(ExprIdOrStar::Star),
        expr_id.cut().map(ExprIdOrStar::ExprId),
    ))
        .parse(i)
}

pub fn expr_id_or_star_or_bar<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], ExprIdOrStarOrBar, E> {
    alt((
        expr_id.map(ExprIdOrStarOrBar::ExprId),
        ctrl('*').p().map(ExprIdOrStarOrBar::Star),
        ctrl('|').p().map(ExprIdOrStarOrBar::Bar),
    ))
        .parse(i)
}

// supply_spec: "<" expr_id [ "," expr_id ] [ "|" expr_id "," expr_id ] ">"
pub fn opt_supply_spec<'a, E: ET<'a>>(i: &'a [u8]) -> IResult<&[u8], Option<SupplySpec>, E> {
    let supply_spec = ctrl('<')
        .then_cut(
            expr_id
                .then_opt(ctrl(',').then_cut(expr_id))
                .then_opt(
                    ctrl('|')
                        .then_cut(expr_id.then(ctrl(',')).then(expr_id))
                        .map(|(a, ((b, c), d))| (a, b, c, d)),
                )
                .then_cut(ctrl('>')),
        )
        .map(|(a, (((b, c), d), e))| SupplySpec(a, b, c, d, e));
    supply_spec.opt().context("supply spec").parse(i)
}
