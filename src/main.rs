extern crate rust_act;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use nom::error::{convert_error, VerboseError};
use rust_act::*;

use nom::combinator::all_consuming;
use std::{env, fs};

use nom_supreme::error::ErrorTree;

fn lex_and_print_errors(data: &str) -> (Vec<Token>, Vec<(WhitespaceKind, &str)>) {
    let tokenized = lexer::<VerboseError<&str>>(data);

    let print_unterm_comm_error = |comm: &str| {
        let start = comm.as_ptr() as usize - data.as_ptr() as usize;
        let span = start..start + 2;
        Report::build(ReportKind::Error, (), start)
            .with_message(format!("Unterminated block comment"))
            .with_label(
                Label::new(span.clone())
                    .with_message(format!("Unterminated block comment"))
                    .with_color(Color::Yellow),
            )
            .with_label(
                Label::new(span)
                    .with_message(format!("Must be closed before this end of file").fg(Color::Red))
                    .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(&data))
            .unwrap();
    };

    let print_illegal_char_error = |c: &str| {
        let start = c.as_ptr() as usize - data.as_ptr() as usize;
        let span = start..start + c.len();
        Report::build(ReportKind::Error, (), start)
            .with_message(format!("Unexpected character {}", c))
            .with_label(
                Label::new(span.clone())
                    .with_message(format!("Unexpected character {}", c))
                    .with_color(Color::Yellow),
            )
            .finish()
            .print(Source::from(&data))
            .unwrap();
    };
    let print_unterm_str_error = |s: &str| {
        let start = s.as_ptr() as usize - data.as_ptr() as usize;
        let span = start..start + s.len();
        Report::build(ReportKind::Error, (), start)
            .with_message(format!("Unterminated string {}", s))
            .with_label(
                Label::new(span.clone())
                    .with_message(format!("Unterminated string {}", s))
                    .with_color(Color::Yellow),
            )
            .finish()
            .print(Source::from(&data))
            .unwrap();
    };

    let mut any_err = false;
    let (tokens, final_whitespace) = match tokenized {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            // I think we should be able to tokenize any file, so this should never be used. However. Leave this code in just to be safe
            println!("{}", convert_error(data, e));
            std::process::exit(1)
        }
        Ok(tokens) => {
            let (remainder, (tokens, final_whitespace)) = tokens;
            assert_eq!(remainder.len(), 0); // every character should have been parsed

            tokens.iter().for_each(|tok| {
                match tok.kind {
                    TokenKind::IllegalChar => {
                        print_illegal_char_error(tok.str_);
                        any_err = true;
                    }
                    TokenKind::UntermStr => {
                        print_unterm_str_error(tok.str_);
                        any_err = true;
                    }
                    _ => (),
                };
                tok.leading_space.iter().for_each(|comm| match comm.0 {
                    WhitespaceKind::UntermBlockComment => {
                        print_unterm_comm_error(comm.1);
                        any_err = true;
                    }
                    _ => (),
                });
            });
            final_whitespace.iter().for_each(|comm| match comm.0 {
                WhitespaceKind::UntermBlockComment => {
                    print_unterm_comm_error(comm.1);
                    any_err = true;
                }
                _ => (),
            });

            match any_err {
                true => std::process::exit(1),
                false => (tokens, final_whitespace),
            }
        }
        _ => {
            assert!(false);
            std::process::exit(1)
        }
    };

    (tokens, final_whitespace)
}
//
// fn print_error_tree<'a>(tl: usize, i: i32, e: &'a ErrorTree<&'a [u8]>) -> (usize, &'a ErrorTree<&'a [u8]>) {
//     let indent = (0..2 * i).map(|_| " ").collect::<String>();
//     match e {
//         ErrorTree::Alt(choices) => choices
//             .iter()
//             .map(|choice| print_error_tree(tl, i + 1, choice))
//             .min_by_key(|v| v.0.clone())
//             .unwrap(),
//         ErrorTree::Base { location, kind } => {
//             println!("{} {:?} {:?}", indent, tl - location.len(), kind);
//             (location.len(), &e)
//         }
//         ErrorTree::Stack { base, contexts } => {
//             println!(
//                 "{} {:?}",
//                 indent,
//                 contexts.iter().map(|(l, c)| (tl - l.len(), c)).collect::<Vec<_>>()
//             );
//             print_error_tree(tl, i + 1, &base)
//         }
//     }
// }

fn get_longest_error_idx<'a>(tl: usize, i: i32, e: &'a ErrorTree<&'a [u8]>) -> usize {
    match e {
        ErrorTree::Alt(choices) => choices
            .iter()
            .map(|choice| get_longest_error_idx(tl, i + 1, choice))
            .min_by_key(|v| v.clone())
            .unwrap(),
        ErrorTree::Base { location, kind: _ } => location.len(),
        ErrorTree::Stack { base, contexts: _ } => get_longest_error_idx(tl, i + 1, &base),
    }
}

fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument")).expect("Failed to read file");

    let (tokens, _) = lex_and_print_errors(&src);
    let flat_tokens = flatten_token_list(&tokens);

    let parsed = all_consuming(top_level)(&flat_tokens);
    match parsed {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            // I think we should be able to tokenize any file, so this should never be used. However. Leave this code in just to be safe
            let i = get_longest_error_idx(flat_tokens.len(), 0, &e);
            let bad_token = &tokens[flat_tokens.len() - i];
            let c = &bad_token.str_;
            let start = c.as_ptr() as usize - src.as_ptr() as usize;
            let span = start..start + c.len();
            Report::build(ReportKind::Error, (), start)
                .with_message(format!("Unexpected token {}", c))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!("Unexpected token {}", c))
                        .with_color(Color::Yellow),
                )
                .finish()
                .print(Source::from(&src))
                .unwrap();
        }
        Ok(_) => {
            println!("Successful parse!")
        }

        _ => panic!("{:?}", parsed),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        let data = "  { \"a\"\t: 42,\n\"b\": [ \"x\", \"y\", 12 ] ,\n\"c\": { \"hello\" : \"world\" //  ";
        let (tokens, _) = lex_and_print_errors(data);
        let flat_tokens = flatten_token_list(&tokens);
        assert_eq!(flat_tokens.len(), 21);
    }
}
