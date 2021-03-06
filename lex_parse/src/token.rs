use std::str;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take, take_until, take_while, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, hex_digit1, one_of},
    combinator::{all_consuming, map, opt, recognize, rest},
    error::{context, ContextError, ParseError},
    IResult,
    multi::{many0, many0_count, many1},
    sequence::{delimited, pair, preceded, tuple},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Num = 0,
    Ident,
    Str,
    // keywords
    KwBool,
    KwChan,
    KwChp,
    KwDataflow,
    KwDataflowCluster,
    KwDefCell,
    KwDefChan,
    KwDefData,
    KwDefEnum,
    KwDefProc,
    KwDefType,
    KwElse,
    KwEnsures,
    KwEnum,
    KwExport,
    KwFunction,
    KwHse,
    KwInitialize,
    KwInt,
    KwInts,
    KwImport,
    KwInterface,
    KwMacro,
    KwMethods,
    KwNamespace,
    KwOpen,
    KwPassN,
    KwPassP,
    KwPBool,
    KwPInt,
    KwPReal,
    KwPrs,
    KwPType,
    KwRefine,
    KwRequires,
    KwSizing,
    KwSkip,
    KwSpec,
    KwTemplate,
    KwTiming,
    KwTransgate,
    // all the control symbols. This allows for easy conversion to/from u8, which makes the parsing really fast
    CtrlLeftParen,
    CtrlRightParen,
    CtrlLeftSquareBracket,
    CtrlRightSquareBracket,
    CtrlLeftCurlyBracket,
    CtrlRightCurlyBracket,
    CtrlSemi,
    CtrlComma,
    CtrlColon,
    CtrlQuestionMark,
    CtrlExclamationMark,
    CtrlDot,
    CtrlPlus,
    CtrlMinus,
    CtrlStar,
    CtrlSlash,
    CtrlEquals,
    CtrlVertBar,
    CtrlAmpersand,
    CtrlTilde,
    CtrlCarrot,
    CtrlMod,
    CtrlLessThan,
    CtrlGreaterThan,
    CtrlPound,
    CtrlBackslash,
    CtrlApostrophy,
    CtrlAtSign,
    CtrlDollarSign,
    // We also need to track whether there is a space before a given Ctrl token, so we have another copy of each name encoding that there IS NOT a leading space
    UnspacedCtrlLeftParen,
    UnspacedCtrlRightParen,
    UnspacedCtrlLeftSquareBracket,
    UnspacedCtrlRightSquareBracket,
    UnspacedCtrlLeftCurlyBracket,
    UnspacedCtrlRightCurlyBracket,
    UnspacedCtrlSemi,
    UnspacedCtrlComma,
    UnspacedCtrlColon,
    UnspacedCtrlQuestionMark,
    UnspacedCtrlExclamationMark,
    UnspacedCtrlDot,
    UnspacedCtrlPlus,
    UnspacedCtrlMinus,
    UnspacedCtrlStar,
    UnspacedCtrlSlash,
    UnspacedCtrlEquals,
    UnspacedCtrlVertBar,
    UnspacedCtrlAmpersand,
    UnspacedCtrlTilde,
    UnspacedCtrlCarrot,
    UnspacedCtrlMod,
    UnspacedCtrlLessThan,
    UnspacedCtrlGreaterThan,
    UnspacedCtrlPound,
    UnspacedCtrlBackslash,
    UnspacedCtrlApostrophy,
    UnspacedCtrlAtSign,
    UnspacedCtrlDollarSign,
    // IllegalChar should be last!
    IllegalChar,
    UntermStr,
}

const CTRL_CHARS: &str = "()[]{};,?!.+-*/=|&~^%<>:#'@$";

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Unspaced {
    No,
    Yes,
}

impl TokenKind {
    pub fn from_identlike(s: &str) -> Self {
        match s {
            "bool" => TokenKind::KwBool,
            "chan" => TokenKind::KwChan,
            "chp" => TokenKind::KwChp,
            "dataflow" => TokenKind::KwDataflow,
            "dataflow_cluster" => TokenKind::KwDataflowCluster,
            "defcell" => TokenKind::KwDefCell,
            "defchan" => TokenKind::KwDefChan,
            "defdata" => TokenKind::KwDefData,
            "defenum" => TokenKind::KwDefEnum,
            "defproc" => TokenKind::KwDefProc,
            "deftype" => TokenKind::KwDefType,
            "else" => TokenKind::KwElse,
            "ensures" => TokenKind::KwEnsures,
            "enum" => TokenKind::KwEnum,
            "export" => TokenKind::KwExport,
            "function" => TokenKind::KwFunction,
            "hse" => TokenKind::KwHse,
            "initialize" => TokenKind::KwInitialize,
            "Initialize" => TokenKind::KwInitialize,
            "int" => TokenKind::KwInt,
            "ints" => TokenKind::KwInts,
            "import" => TokenKind::KwImport,
            "interface" => TokenKind::KwInterface,
            "macro" => TokenKind::KwMacro,
            "methods" => TokenKind::KwMethods,
            "namespace" => TokenKind::KwNamespace,
            "open" => TokenKind::KwOpen,
            "passn" => TokenKind::KwPassN,
            "passp" => TokenKind::KwPassP,
            "pbool" => TokenKind::KwPBool,
            "pint" => TokenKind::KwPInt,
            "preal" => TokenKind::KwPReal,
            "prs" => TokenKind::KwPrs,
            "ptype" => TokenKind::KwPType,
            "refine" => TokenKind::KwRefine,
            "requires" => TokenKind::KwRequires,
            "sizing" => TokenKind::KwSizing,
            "skip" => TokenKind::KwSkip,
            "spec" => TokenKind::KwSpec,
            "template" => TokenKind::KwTemplate,
            "timing" => TokenKind::KwTiming,
            "transgate" => TokenKind::KwTransgate,

            "true" => TokenKind::Num,
            "false" => TokenKind::Num,

            _ => TokenKind::Ident,
        }
    }
    pub const fn from_ctrl(c: char, unspaced: Unspaced) -> Self {
        use TokenKind::*;
        use Unspaced::*;
        match unspaced {
            No => match c {
                '(' => CtrlLeftParen,
                ')' => CtrlRightParen,
                '[' => CtrlLeftSquareBracket,
                ']' => CtrlRightSquareBracket,
                '{' => CtrlLeftCurlyBracket,
                '}' => CtrlRightCurlyBracket,
                ';' => CtrlSemi,
                ',' => CtrlComma,
                ':' => CtrlColon,
                '?' => CtrlQuestionMark,
                '!' => CtrlExclamationMark,
                '.' => CtrlDot,
                '+' => CtrlPlus,
                '-' => CtrlMinus,
                '*' => CtrlStar,
                '/' => CtrlSlash,
                '=' => CtrlEquals,
                '|' => CtrlVertBar,
                '&' => CtrlAmpersand,
                '~' => CtrlTilde,
                '^' => CtrlCarrot,
                '%' => CtrlMod,
                '<' => CtrlLessThan,
                '>' => CtrlGreaterThan,
                '#' => CtrlPound,
                '\\' => CtrlBackslash,
                '\'' => CtrlApostrophy,
                '@' => CtrlAtSign,
                '$' => CtrlDollarSign,
                _ => {
                    // Currently unstable to assert in a const function :(
                    // assert!(false);
                    IllegalChar
                }
            },
            Yes => match c {
                '(' => UnspacedCtrlLeftParen,
                ')' => UnspacedCtrlRightParen,
                '[' => UnspacedCtrlLeftSquareBracket,
                ']' => UnspacedCtrlRightSquareBracket,
                '{' => UnspacedCtrlLeftCurlyBracket,
                '}' => UnspacedCtrlRightCurlyBracket,
                ';' => UnspacedCtrlSemi,
                ',' => UnspacedCtrlComma,
                ':' => UnspacedCtrlColon,
                '?' => UnspacedCtrlQuestionMark,
                '!' => UnspacedCtrlExclamationMark,
                '.' => UnspacedCtrlDot,
                '+' => UnspacedCtrlPlus,
                '-' => UnspacedCtrlMinus,
                '*' => UnspacedCtrlStar,
                '/' => UnspacedCtrlSlash,
                '=' => UnspacedCtrlEquals,
                '|' => UnspacedCtrlVertBar,
                '&' => UnspacedCtrlAmpersand,
                '~' => UnspacedCtrlTilde,
                '^' => UnspacedCtrlCarrot,
                '%' => UnspacedCtrlMod,
                '<' => UnspacedCtrlLessThan,
                '>' => UnspacedCtrlGreaterThan,
                '#' => UnspacedCtrlPound,
                '\\' => UnspacedCtrlBackslash,
                '\'' => UnspacedCtrlApostrophy,
                '@' => UnspacedCtrlAtSign,
                '$' => UnspacedCtrlDollarSign,
                _ => {
                    // Currently unstable to assert in a const function :(
                    // assert!(false);
                    IllegalChar
                }
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum WhitespaceKind {
    Space,
    LineComment,
    BlockComment,
    UntermBlockComment,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub leading_space: Vec<(WhitespaceKind, &'a str)>,
    pub kind: TokenKind,
    pub str_: &'a str,
}

fn whitespace_or_comment<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<(WhitespaceKind, &'a str)>, E> {
    let sp = map(take_while1(move |c| " \t\r\n".contains(c)), |v| {
        (WhitespaceKind::Space, v)
    });
    let line_comment = map(recognize(pair(tag("//"), take_while(|ch| ch != '\n'))), |v| {
        (WhitespaceKind::LineComment, v)
    });
    let block_comment = map(
        recognize(delimited(
            tag("/*"),
            context("block comment", take_until("*/")),
            tag("*/"),
        )),
        |v| (WhitespaceKind::BlockComment, v),
    );
    let unterm_comment = map(recognize(preceded(tag("/*"), rest)), |v| {
        (WhitespaceKind::UntermBlockComment, v)
    });
    many0(alt((line_comment, block_comment, unterm_comment, sp)))(i)
}

fn padded_token<'a, E: ParseError<&'a str> + ContextError<&'a str>>(i: &'a str) -> IResult<&'a str, Token<'a>, E> {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    enum SimpleKind {
        Num,
        Str,
        UntermStr,
        Identlike,
        Ctrl,
        IllegalChar,
    }

    let int16 = map(recognize(preceded(tag("0x"), hex_digit1)), |v| (SimpleKind::Num, v));
    let int2 = map(recognize(preceded(tag("0b"), many1(one_of("01")))), |v| {
        (SimpleKind::Num, v)
    });
    // TODO this should add support for exponential notation, and also add better error handling
    // (because cut doesnt produce great errors)
    let float = map(
        recognize(tuple((
            digit1,
            char('.'),
            digit1,
            // opt(tuple((
            //     alt((char('e'), char('E'))),
            //     opt(alt((char('+'), char('-')))),
            //     cut(digit1),
            // ))),
        ))),
        |v| (SimpleKind::Num, v),
    );
    let int10 = map(recognize(digit1), |v| (SimpleKind::Num, v));
    let ctrl = map(recognize(one_of(CTRL_CHARS)), |v: &'a str| {
        assert!(v.len() == 1);
        (SimpleKind::Ctrl, v)
    });

    // TODO handle escaped character, and handle error cases
    let string = map(
        recognize(tuple((char('"'), opt(is_not("\"\n")), char('"')))),
        |v: &'a str| (SimpleKind::Str, v),
    );
    let unterm_string = map(
        recognize(tuple((char('"'), opt(is_not("\"\n")), char('\n')))),
        |v: &'a str| (SimpleKind::UntermStr, v),
    );

    //   // A parser for identifiers and keywords
    let ident = map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |v: &'a str| (SimpleKind::Identlike, v),
    );

    let err_char = map(take(1usize), |v| (SimpleKind::IllegalChar, v));

    map(
        pair(
            whitespace_or_comment,
            alt((ctrl, int16, int2, float, int10, string, unterm_string, ident, err_char)),
        ),
        |(leading_space, (kind, str_))| {
            let kind = match kind {
                SimpleKind::Num => TokenKind::Num,
                SimpleKind::Str => TokenKind::Str,
                SimpleKind::Identlike => TokenKind::from_identlike(str_),
                SimpleKind::Ctrl => TokenKind::from_ctrl(
                    str_.chars().nth(0).unwrap(),
                    if leading_space.len() == 0 {
                        Unspaced::Yes
                    } else {
                        Unspaced::No
                    },
                ),
                SimpleKind::IllegalChar => TokenKind::IllegalChar,
                SimpleKind::UntermStr => TokenKind::UntermStr,
            };
            Token {
                leading_space,
                kind,
                str_,
            }
        },
    )(i)
}

#[inline(never)]
pub fn lexer<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (Vec<Token<'a>>, Vec<(WhitespaceKind, &'a str)>), E> {
    all_consuming(pair(many0(padded_token), whitespace_or_comment))(i)
}

pub type FlatToken = u8;

pub fn flatten_token_list(toks: &Vec<Token>) -> Vec<FlatToken> {
    toks.iter().map(|tok| tok.kind as u8).collect()
}
