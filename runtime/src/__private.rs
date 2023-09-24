//! # DO NOT USE THIS MODULE!
//!
//! This module contains functions for use in the expanded macros produced by rust-sitter.
//! They need to be public so they can be accessed at all (\*cough\* macro hygiene), but
//! they are not intended to actually be called in any other circumstance.

use crate::tree_sitter;

pub fn extract_struct_or_variant<T>(
    node: tree_sitter::Node,
    construct_expr: impl Fn(&mut Option<tree_sitter::TreeCursor>, &mut usize) -> T,
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

pub fn extract_field<T>(
    cursor_opt: &mut Option<tree_sitter::TreeCursor>,
    last_idx: &mut usize,
    field_name: &str,
    leaf_expr: impl Fn(Option<tree_sitter::Node>, &mut usize) -> T,
) -> T {
    if let Some(cursor) = cursor_opt.as_mut() {
        loop {
            let n = cursor.node();
            if let Some(name) = cursor.field_name() {
                if name == field_name {
                    let out = leaf_expr(Some(n), last_idx);

                    if !cursor.goto_next_sibling() {
                        *cursor_opt = None;
                    };

                    *last_idx = n.end_byte();

                    return out;
                } else {
                    return leaf_expr(None, last_idx);
                }
            } else {
                *last_idx = n.end_byte();
            }

            if !cursor.goto_next_sibling() {
                return leaf_expr(None, last_idx);
            }
        }
    } else {
        return leaf_expr(None, last_idx);
    }
}
