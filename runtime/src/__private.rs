//! # DO NOT USE THIS MODULE!
//!
//! This module contains functions for use in the expanded macros produced by rust-sitter.
//! They need to be public so they can be accessed at all (\*cough\* macro hygiene), but
//! they are not intended to actually be called in any other circumstance.

use crate::{tree_sitter, Extract};

pub fn extract_struct_or_variant<T>(
    node: tree_sitter::Node,
    construct_expr: impl FnOnce(&mut Option<tree_sitter::TreeCursor>, &mut usize) -> T,
) -> T {
    let mut parent_cursor = node.walk();
    construct_expr(
        &mut if parent_cursor.goto_first_child() {
            Some(parent_cursor)
        } else {
            None
        },
        &mut node.start_byte(),
    )
}

pub fn extract_field<LT: Extract<T, Arena>, T, Arena>(
    arena: &mut Arena,
    cursor_opt: &mut Option<tree_sitter::TreeCursor>,
    source: &[u8],
    last_idx: &mut usize,
    field_name: &str,
    closure_ref: Option<&LT::LeafFn>,
) -> T {
    if let Some(cursor) = cursor_opt.as_mut() {
        loop {
            let n = cursor.node();
            if let Some(name) = cursor.field_name() {
                if name == field_name {
                    let out = LT::extract(arena, Some(n), source, *last_idx, closure_ref);

                    if !cursor.goto_next_sibling() {
                        *cursor_opt = None;
                    };

                    *last_idx = n.end_byte();

                    return out;
                } else {
                    return LT::extract(arena, None, source, *last_idx, closure_ref);
                }
            } else {
                *last_idx = n.end_byte();
            }

            if !cursor.goto_next_sibling() {
                return LT::extract(arena, None, source, *last_idx, closure_ref);
            }
        }
    } else {
        LT::extract(arena, None, source, *last_idx, closure_ref)
    }
}

pub fn parse<T: Extract<T, Arena>, Arena: Default>(
    input: &str,
    language: impl Fn() -> tree_sitter::Language,
) -> core::result::Result<(T, Arena), Vec<crate::errors::ParseError>> {
    let mut parser = crate::tree_sitter::Parser::new();
    parser.set_language(&language()).unwrap();
    let tree = parser.parse(input, None).unwrap();
    let root_node = tree.root_node();

    if root_node.has_error() {
        let mut errors = vec![];
        crate::errors::collect_parsing_errors(&root_node, input.as_bytes(), &mut errors);

        Err(errors)
    } else {
        let mut arena = Arena::default();
        let root = <T as crate::Extract<_, Arena>>::extract(
            &mut arena,
            Some(root_node),
            input.as_bytes(),
            0,
            None,
        );

        Ok((root, arena))
    }
}
