use std::fs;
use std::path::PathBuf;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Parser;

use act_fmt::print_pretty;
use lex_parse::*;

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

    /// Only parse the file and check for bad syntax constructs
    #[clap(short, long)]
    parse_only: bool,

    // TODO move configuration into a separate file?
    /// maximum line width
    #[clap(short, long, default_value_t = 80)]
    width: usize,
}

fn parse_or_print_errors(src: &str, verbose: bool) -> ParseTree {
    match lex_parse::parse(&src) {
        LexParseResult::LexErrors(errors) => {
            errors.iter().for_each(|(kind, span)| {
                let report = match kind {
                    LexError::IllegalChar => {
                        Report::build(ReportKind::Error, (), span.start)
                            .with_message(format!("Unexpected character {}", &src[span.clone()]))
                            .with_label(
                                Label::new(span.clone())
                                    .with_message(format!("Unexpected character {}", &src[span.clone()]))
                                    .with_color(Color::Yellow),
                            )
                    }
                    LexError::UntermStr => {
                        Report::build(ReportKind::Error, (), span.start)
                            .with_message(format!("Unterminated string {}", &src[span.clone()]))
                            .with_label(
                                Label::new(span.clone())
                                    .with_message(format!("Unterminated string {}", &src[span.clone()]))
                                    .with_color(Color::Yellow),
                            )
                    }
                    LexError::UntermBlockComment => {
                        Report::build(ReportKind::Error, (), span.start)
                            .with_message(format!("Unterminated block comment"))
                            .with_label(
                                Label::new(span.clone())
                                    .with_message(format!("Unterminated block comment"))
                                    .with_color(Color::Yellow),
                            )
                            .with_label(
                                Label::new(span.clone())
                                    .with_message(format!("Must be closed before this end of file").fg(Color::Red))
                                    .with_color(Color::Red),
                            )
                    }
                };
                report
                    .finish()
                    .print(Source::from(&src))
                    .unwrap()
            });
            std::process::exit(1)
        }
        LexParseResult::ParseErrors(errors) => {
            for (span, verbose_error) in errors {
                if verbose {
                    println!("{}", verbose_error);
                }
                // I think we should be able to tokenize any file, so this should never be used. However. Leave this code in just to be safe
                Report::build(ReportKind::Error, (), span.start)
                    .with_message(format!("Unexpected token {}", &src[span.clone()]))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!("Unexpected token {}", &src[span.clone()]))
                            .with_color(Color::Yellow),
                    )
                    .finish()
                    .print(Source::from(&src))
                    .unwrap();
            }
            std::process::exit(1)
        }
        LexParseResult::Ok(parse_result) => parse_result
    }
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

    let parse_result = parse_or_print_errors(&src, args.verbose);

    if !args.parse_only {
        let pretty_str = print_pretty(&parse_result, args.width)
            .unwrap_or("Failed to pretty-print ast".to_string());
        match args.inplace {
            true => fs::write(&args.path, pretty_str).expect("Failed to write file"),
            false => println!("{}", pretty_str),
        }
    }
}
