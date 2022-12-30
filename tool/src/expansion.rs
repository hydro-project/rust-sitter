use std::collections::HashSet;

use rust_sitter_common::*;
use serde_json::{json, Map, Value};
use syn::{parse::Parse, punctuated::Punctuated, *};

fn gen_field(path: String, leaf: Field, field_index: usize, out: &mut Map<String, Value>) -> Value {
    let leaf_type = leaf.ty;

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::leaf));

    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let pattern_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "pattern")
            .map(|p| p.expr.clone())
    });

    let text_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "text")
            .map(|p| p.expr.clone())
    });

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    let (inner_type, is_vec) = try_extract_inner_type(&leaf_type, "Vec", &skip_over);
    let (inner_type, is_option) = try_extract_inner_type(&inner_type, "Option", &skip_over);

    if let Some(Expr::Lit(lit)) = pattern_param {
        if let Lit::Str(s) = &lit.lit {
            out.insert(
                path.clone(),
                json!({
                    "type": "PATTERN",
                    "value": s.value(),
                }),
            );

            json!({
                "type": "SYMBOL",
                "name": path
            })
        } else {
            panic!("Expected string literal for pattern");
        }
    } else if let Some(Expr::Lit(lit)) = text_param {
        if let Lit::Str(s) = &lit.lit {
            out.insert(
                path.clone(),
                json!({
                    "type": "STRING",
                    "value": s.value(),
                }),
            );

            json!({
                "type": "SYMBOL",
                "name": path
            })
        } else {
            panic!("Expected string literal for text");
        }
    } else {
        let symbol_name = if let Type::Path(p) = filter_inner_type(&inner_type, &skip_over) {
            if p.path.segments.len() == 1 {
                p.path.segments[0].ident.to_string()
            } else {
                panic!("Expected a single segment path");
            }
        } else {
            panic!("Expected a path");
        };

        if is_vec {
            if is_option {
                panic!("Option<Vec> is not supported");
            }

            let delimited_attr = leaf
                .attrs
                .iter()
                .find(|attr| attr.path == syn::parse_quote!(rust_sitter::delimited));

            let delimited_params =
                delimited_attr.and_then(|a| a.parse_args_with(FieldThenParams::parse).ok());

            let delimiter_json = delimited_params.map(|p| {
                gen_field(
                    format!("{}_{}", path, "delimiter"),
                    p.field,
                    field_index,
                    out,
                )
            });

            let repeat_attr = leaf
                .attrs
                .iter()
                .find(|attr| attr.path == syn::parse_quote!(rust_sitter::repeat));

            let repeat_params = repeat_attr.and_then(|a| {
                a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
                    .ok()
            });

            let repeat_non_empty = repeat_params
                .and_then(|p| {
                    p.iter()
                        .find(|param| param.path == "non_empty")
                        .map(|p| p.expr.clone())
                })
                .map(|e| e == syn::parse_quote!(true))
                .unwrap_or(false);

            let field_rule = json!({
                "type": "FIELD",
                "name": leaf.ident.as_ref().map(|v| v.to_string()).unwrap_or(format!("{}", field_index)),
                "content": {
                    "type": "SYMBOL",
                    "name": symbol_name,
                }
            });

            if let Some(delimiter_json) = delimiter_json {
                let non_empty = json!({
                    "type": "SEQ",
                    "members": [
                        field_rule,
                        {
                            "type": "REPEAT",
                            "content": {
                                "type": "SEQ",
                                "members": [
                                    delimiter_json,
                                    field_rule,
                                ]
                            }
                        }
                    ]
                });

                if repeat_non_empty {
                    non_empty
                } else {
                    json!({
                        "type": "CHOICE",
                        "members": [
                            // optional
                            {
                                "type": "BLANK"
                            },
                            non_empty
                        ]
                    })
                }
            } else {
                json!({
                    "type": if repeat_non_empty {
                        "REPEAT1"
                    } else {
                        "REPEAT"
                    },
                    "content": field_rule
                })
            }
        } else {
            json!({
                "type": "SYMBOL",
                "name": symbol_name,
            })
        }
    }
}

fn gen_struct_or_variant(
    path: String,
    attrs: Vec<Attribute>,
    fields: Fields,
    out: &mut Map<String, Value>,
) {
    let children = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let ident_str = field
                .ident
                .as_ref()
                .map(|v| v.to_string())
                .unwrap_or(format!("{}", i));

            let mut skip_over = HashSet::new();
            skip_over.insert("Spanned");
            skip_over.insert("Box");

            let (_, is_vec) = try_extract_inner_type(&field.ty, "Vec", &skip_over);
            let (_, is_option) = try_extract_inner_type(&field.ty, "Option", &skip_over);

            let field_contents = gen_field(
                format!("{}_{}", path.clone(), ident_str),
                field.clone(),
                i,
                out,
            );

            if is_vec {
                if is_option {
                    panic!("Vec<Option<_>> is not supported");
                }

                field_contents
            } else {
                let core = json!({
                    "type": "FIELD",
                    "name": ident_str,
                    "content": field_contents
                });

                if is_option {
                    json!({
                        "type": "CHOICE",
                        "members": [
                            {
                                "type": "BLANK"
                            },
                            core
                        ]
                    })
                } else {
                    core
                }
            }
        })
        .collect::<Vec<Value>>();

    let prec_left_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec_left));

    let prec_left_param = prec_left_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

    let prec_right_attr = attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::prec_right));

    let prec_right_param = prec_right_attr.and_then(|a| a.parse_args_with(Expr::parse).ok());

    let seq_rule = json!({
        "type": "SEQ",
        "members": children
    });

    let rule = if let Some(Expr::Lit(lit)) = prec_left_param {
        if prec_right_attr.is_some() {
            panic!("prec_left and prec_right cannot both be specified");
        }

        if let Lit::Int(i) = &lit.lit {
            json!({
                "type": "PREC_LEFT",
                "value": i.base10_parse::<u32>().unwrap(),
                "content": seq_rule
            })
        } else {
            panic!("Expected integer literal for precedence");
        }
    } else if let Some(Expr::Lit(lit)) = prec_right_param {
        if let Lit::Int(i) = &lit.lit {
            json!({
                "type": "PREC_RIGHT",
                "value": i.base10_parse::<u32>().unwrap(),
                "content": seq_rule
            })
        } else {
            panic!("Expected integer literal for precedence");
        }
    } else {
        seq_rule
    };

    out.insert(path, rule);
}

pub fn generate_grammar(module: &ItemMod) -> Value {
    let mut rules_map = Map::new();
    // for some reason, source_file must be the first key for things to work
    rules_map.insert("source_file".to_string(), json!({}));

    let mut extras_list = vec![];

    let grammar_name = module
        .attrs
        .iter()
        .find_map(|a| {
            if a.path == syn::parse_quote!(rust_sitter::grammar) {
                let grammar_name_expr = a.parse_args_with(Expr::parse).ok();
                if let Some(Expr::Lit(ExprLit {
                    attrs: _,
                    lit: Lit::Str(s),
                })) = grammar_name_expr
                {
                    Some(s.value())
                } else {
                    panic!("Expected string literal for grammar name");
                }
            } else {
                None
            }
        })
        .expect("Each grammar must have a name");

    let (_, contents) = module.content.as_ref().unwrap();

    let root_type = contents
        .iter()
        .find_map(|item| match item {
            Item::Enum(ItemEnum { ident, attrs, .. })
            | Item::Struct(ItemStruct { ident, attrs, .. }) => {
                if attrs
                    .iter()
                    .any(|attr| attr.path == syn::parse_quote!(rust_sitter::language))
                {
                    Some(ident.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
        .expect("Each parser must have the root type annotated with `#[rust_sitter::language]`")
        .to_string();

    contents.iter().for_each(|c| {
        let (symbol, attrs) = match c {
            Item::Enum(e) => {
                e.variants.iter().for_each(|v| {
                    gen_struct_or_variant(
                        format!("{}_{}", e.ident, v.ident),
                        v.attrs.clone(),
                        v.fields.clone(),
                        &mut rules_map,
                    )
                });

                let mut members: Vec<Value> = vec![];
                e.variants.iter().for_each(|v| {
                    let variant_path = format!("{}_{}", e.ident.clone(), v.ident);
                    members.push(json!({
                        "type": "SYMBOL",
                        "name": variant_path
                    }))
                });

                let rule = json!({
                    "type": "CHOICE",
                    "members": members
                });

                rules_map.insert(e.ident.to_string(), rule);

                (e.ident.to_string(), e.attrs.clone())
            }

            Item::Struct(s) => {
                gen_struct_or_variant(
                    s.ident.to_string(),
                    s.attrs.clone(),
                    s.fields.clone(),
                    &mut rules_map,
                );

                (s.ident.to_string(), s.attrs.clone())
            }

            _ => return,
        };

        if attrs
            .iter()
            .any(|a| a.path == syn::parse_quote!(rust_sitter::extra))
        {
            extras_list.push(json!({
                "type": "SYMBOL",
                "name": symbol
            }));
        }
    });

    rules_map.insert(
        "source_file".to_string(),
        rules_map.get(&root_type).unwrap().clone(),
    );

    json!({
        "name": grammar_name,
        "rules": rules_map,
        "extras": extras_list
    })
}
