use std::ops::Range;

use nom::combinator::all_consuming;
use nom::error::{convert_error, VerboseError};
use nom_supreme::error::ErrorTree;

use ast::{FTStart, TopItem};
use items::top_level;
use token::{flatten_token_list, lexer};
pub use token::{FlatToken, Token, TokenKind, WhitespaceKind};

mod token;
mod basic;
mod items;
mod langs;
mod utils;

// short for alias_conn_or_inst
// NOTE: The parsing of these types is closely coupled with "TopItem" and "BaseItem" because of where we
// apply "cut" matters w.r.t the set of items "Connection", "Alias", and "Instance." (and their parsing order)

pub mod ast {
    pub use super::basic::ast::*;
    pub use super::items::ast::*;
    pub use super::langs::ast::*;
    pub use super::utils::SepList1;
}

pub enum LexError {
    IllegalChar,
    UntermStr,
    UntermBlockComment,
}

pub type StrSpan = Range<usize>;
pub type TokSpan = Range<usize>;

pub struct ParseTree<'a> {
    pub ast: Vec<TopItem>,
    pub tokens: Vec<Token<'a>>,
    pub flat_tokens: Vec<FlatToken>,
    pub final_comments: Vec<(WhitespaceKind, &'a str)>,
    pub ft_start: FTStart,
}

pub enum LexParseResult<'a> {
    Ok(ParseTree<'a>),
    LexErrors(Vec<(LexError, StrSpan)>),
    ParseErrors(Vec<(StrSpan, String)>),
}

fn error_tree_to_str<'a>(tl: usize, i: i32, e: &'a ErrorTree<&'a [u8]>) -> (usize, String) {
    let indent = (0..2 * i).map(|_| " ").collect::<String>();
    match e {
        ErrorTree::Alt(choices) => choices
            .iter()
            .map(|choice| error_tree_to_str(tl, i + 1, choice))
            .min_by_key(|v| v.0.clone())
            .unwrap(),
        ErrorTree::Base { location, kind } => {
            let v1 = format!("{} {:?} {:?}\n", indent, tl - location.len(), kind);
            (location.len(), v1)
        }
        ErrorTree::Stack { base, contexts } => {
            let v1 = format!(
                "{} {:?}\n",
                indent,
                contexts.iter().map(|(l, c)| (tl - l.len(), c)).collect::<Vec<_>>()
            );
            let (i, v2) = error_tree_to_str(tl, i + 1, &base);
            (i, v1 + &v2)
        }
    }
}

pub fn parse(data: &str) -> LexParseResult {
    let tokenized = lexer::<VerboseError<&str>>(data);

    let (tokens, final_comments) = match tokenized {
        // I think we should be able to tokenize any file, so this should never be used. However. Leave this code in just to be safe
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            panic!("{}", convert_error(data, e))
        }

        // we are parsing using "complete", so I dont think we should be able to hit this!
        Err(nom::Err::Incomplete(_)) => {
            panic!()
        }

        // Otherwise, we managed to tokenize the stream (as we should be able to), so check if there are any error tokens
        Ok(tokens) => {
            let (remainder, (tokens, final_whitespace)) = tokens;
            assert_eq!(remainder.len(), 0); // every character should have been parsed

            let span_of_str = |s: &str| {
                let start = s.as_ptr() as usize - data.as_ptr() as usize;
                start..(start + s.len())
            };

            let mut errors = Vec::new();
            tokens.iter().for_each(|tok| {
                match tok.kind {
                    TokenKind::IllegalChar => errors.push((LexError::IllegalChar, span_of_str(&tok.str_))),
                    TokenKind::UntermStr => errors.push((LexError::UntermStr, span_of_str(&tok.str_))),
                    _ => (),
                };
                tok.leading_space.iter().for_each(|comm| match comm.0 {
                    WhitespaceKind::UntermBlockComment => errors.push((LexError::UntermBlockComment, span_of_str(&comm.1))),
                    _ => (),
                });
            });
            final_whitespace.iter().for_each(|comm| match comm.0 {
                WhitespaceKind::UntermBlockComment => errors.push((LexError::UntermBlockComment, span_of_str(&comm.1))),
                _ => (),
            });

            match errors.len() {
                0 => (tokens, final_whitespace),
                _ => {
                    return LexParseResult::LexErrors(errors);
                }
            }
        }
    };

    let flat_tokens = flatten_token_list(&tokens);
    let parsed = all_consuming(top_level)(&flat_tokens);

    let ast = match parsed {
        // if the parse was successfully, then just return the ast!
        Ok((i, ast)) => {
            assert_eq!(i.len(), 0);
            ast
        }

        // we should never get an incomplete parse, because we are using "all-consuming"
        Err(nom::Err::Incomplete(_)) => panic!("{:?}", parsed),

        // otherwise, extract and return the error
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            let (i, verbose_error) = error_tree_to_str(flat_tokens.len(), 0, &e);

            let bad_token = &tokens[flat_tokens.len() - i];
            let c = &bad_token.str_;
            let start = c.as_ptr() as usize - data.as_ptr() as usize;
            let span = start..start + c.len();

            return LexParseResult::ParseErrors(vec![
                (span, verbose_error)
            ]);
        }
    };
    let ft_start = FTStart::of_vec(&flat_tokens);
    LexParseResult::Ok(
        ParseTree {
            ast,
            tokens,
            flat_tokens,
            final_comments,
            ft_start,
        }
    )
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
