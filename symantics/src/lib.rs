// use std::collections::HashMap;
// use std::fs;
// use std::path::PathBuf;
//
// use lex_parse::{LexParseResult, ParseTree};
//
// pub struct ErrorGraph <'a> {
//     files: Vec<(/*file content*/ String,LexParseResult <'a>)>,
//     links: HashMap</*path*/ String, usize /*idx*/>
// }
// pub struct FileGraph<'a> (ErrorGraph<'a>);
//
//
// /// This recursively reads all the "imports" at the top of the file (anywhere else they are ignored)
// /// and parses the requested file. It then returns a graph containing (a) all the ParseResult and (b)
// /// the mapping from file path to ParseResult objects. Note that more than one path may refer to the
// /// ParseResult object.
// pub fn gather_file_graph<'a>(root_path: &src) -> Result<
//     FileGraph<'a>, ErrorGraph<'a>> {
//     let args: Cli = Cli::parse();
//
//     let src = fs::read_to_string(&args.path).unwrap_or_else(|_| {
//         println!(
//             "Failed to read file '{}'",
//             args.path.to_str().unwrap_or("<ERROR DECODING PATH>")
//         );
//         std::process::exit(1)
//     });
//
//     let parse_result = parse_or_print_errors(&src, args.verbose);
// }
//
//
// #[cfg(test)]
// mod tests {
//     #[test]
//     fn it_works() {
//         let result = 2 + 2;
//         assert_eq!(result, 4);
//     }
// }
