pub use rust_sitter_macro::*;

pub trait Extract {
    fn extract(node: tree_sitter::Node, source: &[u8]) -> Self;
}
