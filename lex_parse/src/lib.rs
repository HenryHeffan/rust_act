use std::ops::Range;

use nom::error::{convert_error, VerboseError};
use nom::Parser;
use nom_supreme::error::ErrorTree;
use nom_supreme::ParserExt;

use ast::{FTStart, TopItem};
use items::top_level;
use token::{flatten_token_list, lexer};
pub use token::{FlatToken, Token, TokenKind, WhitespaceKind};

mod error;
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

    // first try for a "fast-pass" to parse the file successfully
    let parsed = top_level::<'_, error::ErrorIgnorer<'_>>.complete().parse(&flat_tokens);
    match parsed {
        // if the parse was successfully, then just return the ast!
        Ok((i, ast)) => {
            assert_eq!(i.len(), 0);
            let ft_start = FTStart::of_vec(&flat_tokens);
            return LexParseResult::Ok(
                ParseTree {
                    ast,
                    tokens,
                    flat_tokens,
                    final_comments,
                    ft_start,
                }
            );
        }
        _ => {}
    };

    let parsed = top_level.complete().parse(&flat_tokens);
    match parsed {
        Err(nom::Err::Incomplete(_)) => panic!("we shouldnt be able to be incomplete"),
        Ok(_) => panic!("running the parser a second time should only produce an error/failure"),

        // otherwise, extract and return the error
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            let (i, verbose_error) = error_tree_to_str(flat_tokens.len(), 0, &e);

            let bad_token = &tokens[flat_tokens.len() - i];
            let c = &bad_token.str_;
            let start = c.as_ptr() as usize - data.as_ptr() as usize;
            let span = start..start + c.len();

            LexParseResult::ParseErrors(vec![
                (span, verbose_error)
            ])
        }
    }
}

// Check for the minimum set of errors needed to ensure an unambiguous parse. All other errors should
// be detected during semantic analysis. At the moment, there are two things that we check for here.
//
// First, we check that in a do-loop, the sequence of characters `<-` does not appear in the final
// expression (since by default `*[ A:=B<-C ]`) parses as `*[ A := B < (-C) ])
//
// Then we check that `A = B & C` has parenticies to be either `(A = B) & C` or `A = (B & C)`,
// since by default this parses as `(A = B) & C`, but sometime people mean the opposite
//
// pub fn extract_ambiguous_r_arrows(ast: &Vec<TopItem>) -> () {
//     enum AllowRArrow { Yes, No }
//
//     use ast::*;
//     fn visit_top_item(item: &TopItem) -> () {
//         match item {
//             TopItem::Namespace(NamespaceDecl(_,_,_,_,items,_)) => items.iter().for_each(visit_top_item),
//             TopItem::Chp(LangChp(_,_,_,items,_)) => items.iter().map(|items|visit_chp_item_list(items, AllowRArrow::Yes)),
//             TopItem::Hse(LangHse(_,_,_,bodies,_)) => bodies.iter().map(|bodies| match bodies {
//                 HseBodies::Body(items) => visit_chp_item_list(&items.0, AllowRArrow::Yes),
//                 HseBodies::Labeled(bodies) => bodies.items.iter().for_each(|LabeledHseBody(_, _, items,_, _)| visit_chp_item_list(&items.0, AllowRArrow::Yes))
//             }),
//             _ => {}
//         }
//     }
//     fn visit_chp_item_list(item: &ChpItemList, allow_r_arrow: BracketingKind) -> () {
//         use ChpStmt::*;
//         item.0.items.iter().map(|items| items.items.iter().map(
//             |item| match &item.1 {
//                 Assign(AssignStmt(_,_,e)) |SendStmt(e) |RecvStmt(e) => {
//
//                 }
//                 AssignBoolDir(_) |Skip(_)  => {}
//                 DottedCall(_, _, _, _, _, _) |FuncCall(_, _, _, _)| MacroLoop(_) => {}
//                 ParenedBody(_, items, _) => visit_chp_item_list(items, AllowRArrow::Yes),
//                 BracketedStmt(stmt) => match stmt {
//                     ChpBracketedStmt::DetermSelect(_, items, _) |
//                     ChpBracketedStmt::NonDetermSelect(_,items, _) |
//                     ChpBracketedStmt::WhileLoop(_, items, _) => {
//                         items.items.iter().map(| cmd|{
//                             visit_chp_guarded_cmd(cmd, AllowRArrow::Yes);
//                         });
//                     }
//                     ChpBracketedStmt::DoLoop(_, items, expr, _) => {
//                         visit_chp_item_list(items, AllowRArrow::No);
//                         expr.iter().map(|(_, expr)| visit_expr(expr, AllowRArrow::No))
//                     }
//                     ChpBracketedStmt::Wait(_, expr, _) => {
//                         visit_expr(expr, AllowRArrow::No);
//                     }
//                 }
//             }
//         ));
//     }
//     fn visit_chp_guarded_cmd(cmd: &GuardedCmd, allow_r_arrow: AllowRArrow) -> () {
//             match &cmd {
//                 GuardedCmd::Expr(_, _, items)  |
//                 GuardedCmd::Else(_, _, items) |
//                 GuardedCmd::Macro(_, _, _, _, _, _, _, _, items, _) => {
//                     visit_chp_item_list(items, allow_r_arrow)
//                 }
//             }
//     }
//     fn visit_expr(e: &Expr, allow_r_arrow: AllowRArrow) -> () {
//
//     }
//     ast.iter().for_each(|v| visit_top_item(v))
// }

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
