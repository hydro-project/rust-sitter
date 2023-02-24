use std::ops::Deref;

pub use rust_sitter_macro::*;

#[cfg(feature = "tree-sitter-standard")]
pub use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
pub use tree_sitter_runtime_c2rust as tree_sitter;

/// Defines the logic used to convert a node in a Tree Sitter tree to
/// the corresponding Rust type.
pub trait Extract {
    fn extract(node: Option<tree_sitter::Node>, source: &[u8], last_idx: usize) -> Self;
}

impl Extract for () {
    fn extract(_node: Option<tree_sitter::Node>, _source: &[u8], _last_idx: usize) {}
}

impl<T: Extract> Extract for Option<T> {
    fn extract(node: Option<tree_sitter::Node>, source: &[u8], last_idx: usize) -> Option<T> {
        node.map(|n| Extract::extract(Some(n), source, last_idx))
    }
}

impl<T: Extract> Extract for Box<T> {
    fn extract(node: Option<tree_sitter::Node>, source: &[u8], last_idx: usize) -> Self {
        Box::new(Extract::extract(node, source, last_idx))
    }
}

impl<T: Extract> Extract for Vec<T> {
    fn extract(node: Option<tree_sitter::Node>, source: &[u8], mut last_idx: usize) -> Self {
        node.map(|node| {
            let mut cursor = node.walk();
            let mut out = vec![];
            if cursor.goto_first_child() {
                loop {
                    let n = cursor.node();
                    if cursor.field_name().is_some() {
                        out.push(Extract::extract(Some(n), source, last_idx));
                    }

                    last_idx = n.end_byte();

                    if !cursor.goto_next_sibling() {
                        break;
                    }
                }
            }

            out
        })
        .unwrap_or_default()
    }
}

#[derive(Clone, Debug)]
/// A wrapper around a value that also contains the span of the value in the source.
pub struct Spanned<T> {
    /// The underlying parsed node.
    pub value: T,
    /// The span of the node in the source. The first value is the inclusive start
    /// of the span, and the second value is the exclusive end of the span.
    pub span: (usize, usize),
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T: Extract> Extract for Spanned<T> {
    fn extract(node: Option<tree_sitter::Node>, source: &[u8], last_idx: usize) -> Spanned<T> {
        Spanned {
            value: Extract::extract(node, source, last_idx),
            span: node
                .map(|n| (n.start_byte(), n.end_byte()))
                .unwrap_or((last_idx, last_idx)),
        }
    }
}

pub mod errors {
    #[cfg(feature = "tree-sitter-standard")]
    use tree_sitter_runtime_standard as tree_sitter;

    #[cfg(feature = "tree-sitter-c2rust")]
    use tree_sitter_runtime_c2rust as tree_sitter;

    #[derive(Debug)]
    /// An explanation for an error that occurred during parsing.
    pub enum ParseErrorReason {
        /// The parser did not expect to see some token.
        UnexpectedToken(String),
        /// Tree Sitter failed to parse a specific intermediate node.
        /// The underlying failures are in the vector.
        FailedNode(Vec<ParseError>),
        /// The parser expected a specific token, but it was not found.
        MissingToken(String),
    }

    #[derive(Debug)]
    /// An error that occurred during parsing.
    pub struct ParseError {
        pub reason: ParseErrorReason,
        /// Inclusive start of the error.
        pub start: usize,
        /// Exclusive end of the error.
        pub end: usize,
    }

    /// Given the root node of a Tree Sitter parsing result, accumulates all
    /// errors that were emitted.
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
                let contents = node.utf8_text(source).unwrap();
                if !contents.is_empty() {
                    errors.push(ParseError {
                        reason: ParseErrorReason::UnexpectedToken(contents.to_string()),
                        start: node.start_byte(),
                        end: node.end_byte(),
                    })
                } else {
                    errors.push(ParseError {
                        reason: ParseErrorReason::FailedNode(vec![]),
                        start: node.start_byte(),
                        end: node.end_byte(),
                    })
                }
            }
        } else if node.is_missing() {
            errors.push(ParseError {
                reason: ParseErrorReason::MissingToken(node.kind().to_string()),
                start: node.start_byte(),
                end: node.end_byte(),
            })
        } else if node.has_error() {
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .for_each(|c| collect_parsing_errors(&c, source, errors));
        }
    }
}
