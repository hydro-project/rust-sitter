use std::io::Write;

use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use rust_sitter::errors::{ParseError, ParseErrorReason};

#[rust_sitter::grammar]
pub mod arithmetic_grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub enum Expression {
        Number(
            #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse().unwrap())] i32,
        ),
        #[rust_sitter::prec_left(1)]
        Sub(
            Box<Expression>,
            #[rust_sitter::leaf(text = "-", transform = |_v| ())] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec_left(2)]
        Mul(
            Box<Expression>,
            #[rust_sitter::leaf(text = "*", transform = |_v| ())] (),
            Box<Expression>,
        ),
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s", transform = |_v| ())]
        _whitespace: (),
    }
}

fn convert_parse_error_to_diagnostics(
    file_span: &codemap::Span,
    error: &ParseError,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match &error.reason {
        ParseErrorReason::MissingToken(tok) => diagnostics.push(Diagnostic {
            level: Level::Error,
            message: format!("Missing token: \"{}\"", tok),
            code: Some("S000".to_string()),
            spans: vec![SpanLabel {
                span: file_span.subspan(error.start as u64, error.end as u64),
                style: SpanStyle::Primary,
                label: Some(format!("missing \"{}\"", tok)),
            }],
        }),

        ParseErrorReason::UnexpectedToken(tok) => diagnostics.push(Diagnostic {
            level: Level::Error,
            message: format!("Unexpected token: \"{}\"", tok),
            code: Some("S000".to_string()),
            spans: vec![SpanLabel {
                span: file_span.subspan(error.start as u64, error.end as u64),
                style: SpanStyle::Primary,
                label: Some(format!("unexpected \"{}\"", tok)),
            }],
        }),

        ParseErrorReason::FailedNode(errors) => {
            if errors.is_empty() {
                diagnostics.push(Diagnostic {
                    level: Level::Error,
                    message: "Failed to parse node".to_string(),
                    code: Some("S000".to_string()),
                    spans: vec![SpanLabel {
                        span: file_span.subspan(error.start as u64, error.end as u64),
                        style: SpanStyle::Primary,
                        label: Some("failed".to_string()),
                    }],
                })
            } else {
                for error in errors {
                    convert_parse_error_to_diagnostics(file_span, error, diagnostics);
                }
            }
        }
    }
}

fn main() {
    let stdin = std::io::stdin();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            break;
        }

        match arithmetic_grammar::parse(input) {
            Ok(expr) => println!("{:?}", expr),
            Err(errs) => {
                let mut codemap = CodeMap::new();
                let file_span = codemap.add_file("<input>".to_string(), input.to_string());
                let mut diagnostics = vec![];
                for error in errs {
                    convert_parse_error_to_diagnostics(&file_span.span, &error, &mut diagnostics);
                }

                let mut emitter = Emitter::stderr(ColorConfig::Always, Some(&codemap));
                emitter.emit(&diagnostics);
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::arithmetic_grammar;

    #[test]
    fn arithmetic_grammar() {
        // successful parses
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2 - 3"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2 * 3"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 * 2 * 3"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 * 2 - 3"));

        // failed parses
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 + 2"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2 -"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("a1"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1a"));
    }
}
