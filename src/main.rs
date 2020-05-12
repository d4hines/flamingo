#[macro_use]
extern crate lalrpop_util;

mod ast;
mod parse_tests;
mod print;

use lalrpop_util::*;
use print::*;
use std::env;
use std::fs;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term;

lalrpop_mod!(pub parse);

fn main() {
    let alm_file = env::args()
        .nth(1)
        .expect("Please specify an ALM module to compile.");

    let read_file_error = format!("Could not read file {}!", alm_file);
    let contents = fs::read_to_string(&alm_file).expect(read_file_error.as_str());

    let mut files = SimpleFiles::new();

    let file_id = files.add(alm_file.clone(), &contents);

    let parse_result = parse::ALMModuleParser::new().parse(contents.as_str());
    match parse_result {
        Ok(ast) => {
            println!("{}", ast.to_ddlog());
            eprintln!("ALM module compiled successfully!
             .-.
            ((`-)
             \\\\
              \\\\
       .=\"\"\"=._))
      /  .,   .'
     /__(,_.-'
    `    /|
        /_|__
          | `))
          |
         -\"==
");
            std::process::exit(0);
        }
        Err(err) => match err {
            ParseError::UnrecognizedToken { token, expected } => {
                let mut expected_str = "Expected one of:\n\t".to_owned();
                expected_str.push_str(expected.join(",\n\t").as_str());

                let (start, _, end) = token;
                let diagnostic = Diagnostic::error()
                    .with_message("Parse error: Unexpected token.")
                    .with_code("E0001")
                    .with_labels(vec![
                        Label::primary(file_id, start..end).with_message("unexpected token")
                    ])
                    .with_notes(vec![expected_str]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                    .unwrap();
                std::process::exit(1);
            },
            ParseError::InvalidToken{location} => {
                let diagnostic = Diagnostic::error()
                    .with_message("Parse error: Invalid token.")
                    .with_code("E0002")
                    .with_labels(vec![
                        Label::primary(file_id, location..location).with_message("unexpected token")
                    ]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                    .unwrap();

                std::process::exit(1);
            }
            _ => {
                eprintln!("{:?}", err);
                std::process::exit(1);
            },
        }
    }
}
