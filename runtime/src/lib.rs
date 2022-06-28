pub use rust_sitter_macro::*;

pub trait Extract {
    fn extract(node: tree_sitter::Node, source: &[u8]) -> Self;
}

pub mod errors {
    #[derive(Debug)]
    pub enum ParseErrorReason {
        UnexpectedToken(String),
        FailedNode(Vec<ParseError>),
        MissingToken(String),
    }

    #[derive(Debug)]
    pub struct ParseError {
        pub reason: ParseErrorReason,
        /// Inclusive start of the error.
        pub start: usize,
        /// Exclusive end of the error.
        pub end: usize,
    }

    pub fn collect_parsing_errors(
        node: &tree_sitter::Node,
        source: &[u8],
        errors: &mut Vec<ParseError>,
    ) {
        if node.is_error() {
            if node.child(0).is_some() {
                // we managed to parse some children, so collect underlying errors for this node
                let mut inner_errors = vec![];
                let mut cursor = node.walk();
                node.children(&mut cursor)
                    .for_each(|c| collect_parsing_errors(&c, source, &mut inner_errors));

                errors.push(ParseError {
                    reason: ParseErrorReason::FailedNode(inner_errors),
                    start: node.start_byte(),
                    end: node.end_byte(),
                })
            } else {
                let sexp = node.to_sexp();
                if sexp.starts_with("(UNEXPECTED") {
                    let mut tok_getter = sexp.chars();
                    for _ in 0.."(UNEXPECTED '".len() {
                        tok_getter.next();
                    }
                    for _ in 0.."')".len() {
                        tok_getter.next_back();
                    }
                    let tok = tok_getter.as_str();

                    errors.push(ParseError {
                        reason: ParseErrorReason::UnexpectedToken(tok.to_string()),
                        start: node.start_byte(),
                        end: node.end_byte(),
                    })
                } else {
                    panic!("Unknown error type: {}", sexp);
                }
            }
        } else if node.is_missing() {
            errors.push(ParseError {
                reason: ParseErrorReason::MissingToken(node.kind().to_string()),
                start: node.start_byte(),
                end: node.end_byte(),
            })
        } else {
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .for_each(|c| collect_parsing_errors(&c, source, errors));
        }
    }
}
