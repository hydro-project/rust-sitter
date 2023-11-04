pub mod __private;

use std::ops::Deref;

pub use rust_sitter_macro::*;

#[cfg(feature = "tree-sitter-standard")]
pub use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
pub use tree_sitter_runtime_c2rust as tree_sitter;

/// Defines the logic used to convert a node in a Tree Sitter tree to
/// the corresponding Rust type.
pub trait Extract<Output> {
    type LeafFn: ?Sized;
    fn extract(
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Output;
}

pub struct WithLeaf<L> {
    _phantom: std::marker::PhantomData<L>,
}

impl<L> Extract<L> for WithLeaf<L> {
    type LeafFn = dyn Fn(&str) -> L;

    fn extract(
        node: Option<tree_sitter::Node>,
        source: &[u8],
        _last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> L {
        node.and_then(|n| n.utf8_text(source).ok())
            .map(|s| leaf_fn.unwrap()(s))
            .unwrap()
    }
}

impl Extract<()> for () {
    type LeafFn = ();
    fn extract(
        _node: Option<tree_sitter::Node>,
        _source: &[u8],
        _last_idx: usize,
        _leaf_fn: Option<&Self::LeafFn>,
    ) {
    }
}

impl<T: Extract<U>, U> Extract<Option<U>> for Option<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Option<U> {
        node.map(|n| T::extract(Some(n), source, last_idx, leaf_fn))
    }
}

impl<T: Extract<U>, U> Extract<Box<U>> for Box<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Box<U> {
        Box::new(T::extract(node, source, last_idx, leaf_fn))
    }
}

impl<T: Extract<U>, U> Extract<Vec<U>> for Vec<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        node: Option<tree_sitter::Node>,
        source: &[u8],
        mut last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Vec<U> {
        node.map(|node| {
            let mut cursor = node.walk();
            let mut out = vec![];
            if cursor.goto_first_child() {
                loop {
                    let n = cursor.node();
                    if cursor.field_name().is_some() {
                        out.push(T::extract(Some(n), source, last_idx, leaf_fn));
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

impl<T: Extract<U>, U> Extract<Spanned<U>> for Spanned<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Spanned<U> {
        Spanned {
            value: T::extract(node, source, last_idx, leaf_fn),
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
