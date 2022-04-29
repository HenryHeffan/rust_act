extern crate mylib;
use mylib::*;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Parser;
use mylib::ast::TopItem;
use nom::combinator::all_consuming;
use nom::error::{convert_error, VerboseError};
use nom_supreme::error::ErrorTree;
use std::fs;
use std::path::PathBuf;

fn lex_and_print_errors(data: &str) -> (Vec<Token>, Vec<(WhitespaceKind, &str)>) {
    let tokenized = lexer::<VerboseError<&str>>(data);

    let (tokens, final_whitespace) = match tokenized {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            // I think we should be able to tokenize any file, so this should never be used. However. Leave this code in just to be safe
            println!("{}", convert_error(data, e));
            std::process::exit(1)
        }
        Ok(tokens) => {
            let (remainder, (tokens, final_whitespace)) = tokens;
            assert_eq!(remainder.len(), 0); // every character should have been parsed

            enum ErrorKind {
                IllegalChar,
                UntermStr,
                UntermBlockComment,
            }

            let mut errors = Vec::new();
            tokens.iter().for_each(|tok| {
                match tok.kind {
                    TokenKind::IllegalChar => errors.push((ErrorKind::IllegalChar, &tok.str_)),
                    TokenKind::UntermStr => errors.push((ErrorKind::UntermStr, &tok.str_)),
                    _ => (),
                };
                tok.leading_space.iter().for_each(|comm| match comm.0 {
                    WhitespaceKind::UntermBlockComment => errors.push((ErrorKind::UntermBlockComment, &comm.1)),
                    _ => (),
                });
            });
            final_whitespace.iter().for_each(|comm| match comm.0 {
                WhitespaceKind::UntermBlockComment => errors.push((ErrorKind::UntermBlockComment, &comm.1)),
                _ => (),
            });

            match errors.len() {
                0 => (tokens, final_whitespace),
                _ => {
                    errors.iter().for_each(|(kind, s)| match kind {
                        ErrorKind::IllegalChar => {
                            let start = s.as_ptr() as usize - data.as_ptr() as usize;
                            let span = start..start + s.len();
                            Report::build(ReportKind::Error, (), start)
                                .with_message(format!("Unexpected character {}", s))
                                .with_label(
                                    Label::new(span.clone())
                                        .with_message(format!("Unexpected character {}", s))
                                        .with_color(Color::Yellow),
                                )
                                .finish()
                                .print(Source::from(&data))
                                .unwrap();
                        }
                        ErrorKind::UntermStr => {
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
                        }
                        ErrorKind::UntermBlockComment => {
                            let start = s.as_ptr() as usize - data.as_ptr() as usize;
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
                        }
                    });
                    std::process::exit(1)
                }
            }
        }
        _ => {
            assert!(false);
            std::process::exit(1)
        }
    };

    (tokens, final_whitespace)
}

fn print_error_tree<'a>(tl: usize, i: i32, e: &'a ErrorTree<&'a [u8]>) -> (usize, &'a ErrorTree<&'a [u8]>) {
    let indent = (0..2 * i).map(|_| " ").collect::<String>();
    match e {
        ErrorTree::Alt(choices) => choices
            .iter()
            .map(|choice| print_error_tree(tl, i + 1, choice))
            .min_by_key(|v| v.0.clone())
            .unwrap(),
        ErrorTree::Base { location, kind } => {
            println!("{} {:?} {:?}", indent, tl - location.len(), kind);
            (location.len(), &e)
        }
        ErrorTree::Stack { base, contexts } => {
            println!(
                "{} {:?}",
                indent,
                contexts.iter().map(|(l, c)| (tl - l.len(), c)).collect::<Vec<_>>()
            );
            print_error_tree(tl, i + 1, &base)
        }
    }
}

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

fn parse_and_print_errors(
    data: &str,
    verbose: bool,
) -> (Vec<TopItem>, Vec<Token>, Vec<FlatToken>, Vec<(WhitespaceKind, &str)>) {
    let (tokens, final_whitespace) = lex_and_print_errors(data);

    let flat_tokens = flatten_token_list(&tokens);
    let parsed = all_consuming(top_level)(&flat_tokens);

    let ast = match parsed {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            if verbose {
                print_error_tree(flat_tokens.len(), 0, &e);
            }
            // I think we should be able to tokenize any file, so this should never be used. However. Leave this code in just to be safe
            let i = get_longest_error_idx(flat_tokens.len(), 0, &e);
            let bad_token = &tokens[flat_tokens.len() - i];
            let c = &bad_token.str_;
            let start = c.as_ptr() as usize - data.as_ptr() as usize;
            let span = start..start + c.len();
            Report::build(ReportKind::Error, (), start)
                .with_message(format!("Unexpected token {}", c))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!("Unexpected token {}", c))
                        .with_color(Color::Yellow),
                )
                .finish()
                .print(Source::from(&data))
                .unwrap();
            std::process::exit(1)
        }
        Ok((_, ast)) => ast,
        _ => panic!("{:?}", parsed),
    };

    (ast, tokens, flat_tokens, final_whitespace)
}

// Note that the three-slash comments are actually used in the help output
/// A tool to format Act code.
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    // TODO allow multiple paths
    /// path of the file to update
    #[clap()]
    path: PathBuf,

    /// overwrite the file with the formatted code
    #[clap(short, long)]
    inplace: bool,

    /// overwrite the file with the formatted code
    #[clap(short, long)]
    verbose: bool,

    // TODO move configuration into a separate file?
    /// maximum line width
    #[clap(short, long, default_value_t = 80)]
    width: usize,
}

fn main() {
    let args: Cli = Cli::parse();

    let src = fs::read_to_string(&args.path).unwrap_or_else(|_| {
        println!(
            "Failed to read file '{}'",
            args.path.to_str().unwrap_or("<ERROR DECODING PATH>")
        );
        std::process::exit(1)
    });
    let (ast, tokens, flat_tokens, final_whitespace) = parse_and_print_errors(&src, args.verbose);

    let pretty_str = print_pretty(&ast, final_whitespace, &flat_tokens, &tokens, args.width)
        .unwrap_or("Failed to pretty-print ast".to_string());
    match args.inplace {
        true => fs::write(&args.path, pretty_str).expect("Failed to write file"),
        false => println!("{}", pretty_str),
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
