pub mod __private;

use std::ops::Deref;

pub use rust_sitter_macro::*;

#[cfg(feature = "tree-sitter-standard")]
pub use tree_sitter_runtime_standard as tree_sitter;

#[cfg(feature = "tree-sitter-c2rust")]
pub use tree_sitter_runtime_c2rust as tree_sitter;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BadHandleError;

impl std::fmt::Display for BadHandleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Bad handle")
    }
}
impl std::error::Error for BadHandleError {}

/// A strongly typed reference to an [`Arena`] item.
pub struct Handle<T> {
    index: usize,
    marker: std::marker::PhantomData<T>,
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Handle<T> {}

impl<T> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> std::fmt::Debug for Handle<T> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "[{}]", self.index)
    }
}

impl<T> std::hash::Hash for Handle<T> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.index.hash(hasher)
    }
}

impl<T> Handle<T> {
    pub(crate) const fn new(index: usize) -> Self {
        Handle {
            index,
            marker: std::marker::PhantomData,
        }
    }
}

/// An arena holding something parsed.
///
/// Adding new items to the arena produces a strongly-typed [`Handle`].
/// The arena can be indexed using the given handle to obtain
/// a reference to the stored item.
#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Arena<T> {
    /// Values of this arena.
    data: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T> Arena<T> {
    /// Create a new arena with no initial capacity allocated.
    pub const fn new() -> Self {
        Arena { data: Vec::new() }
    }

    /// Extracts the inner vector.
    #[allow(clippy::missing_const_for_fn)] // ignore due to requirement of #![feature(const_precise_live_drops)]
    pub fn into_inner(self) -> Vec<T> {
        self.data
    }

    /// Returns the current number of items stored in this arena.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the arena contains no elements.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns an iterator over the items stored in this arena, returning both
    /// the item's handle and a reference to it.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &T)> + ExactSizeIterator {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| (Handle::new(i), v))
    }

    /// Returns a iterator over the items stored in this arena,
    /// returning both the item's handle and a mutable reference to it.
    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (Handle<T>, &mut T)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| (Handle::new(i), v))
    }

    /// Adds a new value to the arena, returning a typed handle.
    pub fn append(&mut self, value: T) -> Handle<T> {
        let index = self.data.len();
        self.data.push(value);
        Handle::new(index)
    }

    /// Fetch a handle to an existing type.
    pub fn fetch_if<F: Fn(&T) -> bool>(&self, fun: F) -> Option<Handle<T>> {
        self.data
            .iter()
            .position(fun)
            .map(|index| Handle::new(index))
    }

    /// Adds a value with a custom check for uniqueness:
    /// returns a handle pointing to
    /// an existing element if the check succeeds, or adds a new
    /// element otherwise.
    pub fn fetch_if_or_append<F: Fn(&T, &T) -> bool>(&mut self, value: T, fun: F) -> Handle<T> {
        if let Some(index) = self.data.iter().position(|d| fun(d, &value)) {
            Handle::new(index)
        } else {
            self.append(value)
        }
    }

    /// Adds a value with a check for uniqueness, where the check is plain comparison.
    pub fn fetch_or_append(&mut self, value: T) -> Handle<T>
    where
        T: PartialEq,
    {
        self.fetch_if_or_append(value, T::eq)
    }

    pub fn try_get(&self, handle: Handle<T>) -> Result<&T, BadHandleError> {
        self.data.get(handle.index).ok_or(BadHandleError)
    }

    /// Get a mutable reference to an element in the arena.
    pub fn get_mut(&mut self, handle: Handle<T>) -> &mut T {
        self.data.get_mut(handle.index).unwrap()
    }

    /// Clears the arena keeping all allocations
    pub fn clear(&mut self) {
        self.data.clear()
    }

    /// Assert that `handle` is valid for this arena.
    pub fn check_contains_handle(&self, handle: Handle<T>) -> Result<(), BadHandleError> {
        if handle.index < self.data.len() {
            Ok(())
        } else {
            Err(BadHandleError)
        }
    }

    /// Assert that `range` is valid for this arena.
    pub fn check_contains_range(
        &self,
        range: &std::ops::Range<Handle<T>>,
    ) -> Result<(), BadHandleError> {
        // Since `range.inner` is a `Range<u32>`, we only need to check that the
        // start precedes the end, and that the end is in range.
        if range.start > range.end {
            return Err(BadHandleError);
        }

        // Empty ranges are tolerated: they can be produced by compaction.
        if range.start == range.end {
            return Ok(());
        }

        let last_handle = Handle::new(range.end.index - 1);
        if self.check_contains_handle(last_handle).is_err() {
            return Err(BadHandleError);
        }

        Ok(())
    }
}

impl<T> std::ops::Index<Handle<T>> for Arena<T> {
    type Output = T;
    fn index(&self, handle: Handle<T>) -> &T {
        &self.data[handle.index]
    }
}

impl<T> std::ops::IndexMut<Handle<T>> for Arena<T> {
    fn index_mut(&mut self, handle: Handle<T>) -> &mut T {
        &mut self.data[handle.index]
    }
}

impl<T> std::ops::Index<std::ops::Range<Handle<T>>> for Arena<T> {
    type Output = [T];
    fn index(&self, range: std::ops::Range<Handle<T>>) -> &[T] {
        &self.data[range.start.index..range.end.index]
    }
}

/// Implemented by an Arena which can be extended with new elements of type `T`.
pub trait ArenaInsert<T> {
    fn append(&mut self, value: T) -> Handle<T>;
}

impl<T> ArenaInsert<T> for Arena<T> {
    fn append(&mut self, value: T) -> Handle<T> {
        Arena::append(self, value)
    }
}

/// Defines the logic used to convert a node in a Tree Sitter tree to
/// the corresponding Rust type.
///
/// The arena parameter holds the nodes pointed to by [`Handle`]s.
pub trait Extract<Output, Arena> {
    type LeafFn: ?Sized;
    fn extract(
        arena: &mut Arena,
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Output;
}

pub struct WithLeaf<L> {
    _phantom: std::marker::PhantomData<L>,
}

impl<L, Arena> Extract<L, Arena> for WithLeaf<L> {
    type LeafFn = dyn Fn(&str) -> L;

    fn extract(
        _arena: &mut Arena,
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

impl<Arena> Extract<(), Arena> for () {
    type LeafFn = ();
    fn extract(
        _arena: &mut Arena,
        _node: Option<tree_sitter::Node>,
        _source: &[u8],
        _last_idx: usize,
        _leaf_fn: Option<&Self::LeafFn>,
    ) {
    }
}

impl<T: Extract<U, Arena>, U, Arena> Extract<Option<U>, Arena> for Option<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        arena: &mut Arena,
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Option<U> {
        node.map(|n| T::extract(arena, Some(n), source, last_idx, leaf_fn))
    }
}

impl<T: Extract<U, Arena>, U, Arena> Extract<Box<U>, Arena> for Box<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        arena: &mut Arena,
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Box<U> {
        Box::new(T::extract(arena, node, source, last_idx, leaf_fn))
    }
}

impl<T: Extract<U, Arena>, U, Arena: ArenaInsert<U>> Extract<Handle<U>, Arena> for Handle<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        arena: &mut Arena,
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Handle<U> {
        let v = T::extract(arena, node, source, last_idx, leaf_fn);
        arena.append(v)
    }
}

impl<T: Extract<U, Arena>, U, Arena> Extract<Vec<U>, Arena> for Vec<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        arena: &mut Arena,
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
                        out.push(T::extract(arena, Some(n), source, last_idx, leaf_fn));
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

impl<T: Extract<U, Arena>, U, Arena> Extract<Spanned<U>, Arena> for Spanned<T> {
    type LeafFn = T::LeafFn;
    fn extract(
        arena: &mut Arena,
        node: Option<tree_sitter::Node>,
        source: &[u8],
        last_idx: usize,
        leaf_fn: Option<&Self::LeafFn>,
    ) -> Spanned<U> {
        Spanned {
            value: T::extract(arena, node, source, last_idx, leaf_fn),
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
