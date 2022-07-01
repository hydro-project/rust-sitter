use proc_macro2::Span;
use quote::ToTokens;
use rust_sitter_common::*;
use syn::{parse::Parse, punctuated::Punctuated, *};

fn is_sitter_attr(attr: &Attribute) -> bool {
    let ident = &attr.path.segments.iter().next().unwrap().ident;
    ident == "rust_sitter"
}

enum ParamOrField {
    Param(Expr),
    Field(FieldValue),
}

impl ToTokens for ParamOrField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            ParamOrField::Param(expr) => expr.to_tokens(tokens),
            ParamOrField::Field(field) => field.to_tokens(tokens),
        }
    }
}

fn gen_field(path: String, leaf: Field, out: &mut Vec<Item>) {
    let extract_ident = Ident::new(&format!("extract_{}", path), Span::call_site());
    let leaf_type = leaf.ty;

    let leaf_text_expr: Expr = syn::parse_quote! {
        node.and_then(|n| n.utf8_text(source).ok())
    };

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::leaf));

    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let transform_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "transform")
            .map(|p| p.expr.clone())
    });

    let (inner_type, is_option) = try_extract_inner_type(&leaf_type, "Option");

    let (leaf_stmts, leaf_expr): (Vec<Stmt>, Expr) = match transform_param {
        Some(closure) => (
            vec![syn::parse_quote! {
                fn make_transform() -> impl Fn(&str) -> #inner_type {
                    #closure
                }
            }],
            if is_option {
                syn::parse_quote! {
                    #leaf_text_expr.map(|t| make_transform()(t))
                }
            } else {
                syn::parse_quote! {
                    make_transform()(#leaf_text_expr.unwrap())
                }
            },
        ),
        None => {
            if inner_type == syn::parse_quote!(()) {
                (
                    vec![],
                    if is_option {
                        syn::parse_quote! {
                            node.map(|_n| ())
                        }
                    } else {
                        syn::parse_quote! {
                            ()
                        }
                    },
                )
            } else {
                let (vec_type, is_vec) = try_extract_inner_type(&inner_type, "Vec");
                if is_vec {
                    if is_option {
                        panic!("Option<Vec> is not supported");
                    }

                    let field_name = leaf.ident.unwrap().to_string();

                    (
                        vec![
                            syn::parse_quote! {
                                let node = node.unwrap();
                            },
                            syn::parse_quote! {
                                let mut cursor = node.walk();
                            },
                        ],
                        syn::parse_quote! {
                            node
                                .children_by_field_name(#field_name, &mut cursor)
                                .map(|n| #vec_type::extract(n, source))
                                .collect::<Vec<#vec_type>>()
                        },
                    )
                } else {
                    let (inner_type, is_box) = try_extract_inner_type(&inner_type, "Box");

                    let extracted_inner: Expr = if is_option {
                        syn::parse_quote!(node.map(|n| #inner_type::extract(n, source)))
                    } else {
                        syn::parse_quote!(#inner_type::extract(node.unwrap(), source))
                    };

                    if is_box {
                        (
                            vec![],
                            syn::parse_quote! {
                                Box::new(#extracted_inner)
                            },
                        )
                    } else {
                        (
                            vec![],
                            syn::parse_quote! {
                                #extracted_inner
                            },
                        )
                    }
                }
            }
        }
    };

    out.push(syn::parse_quote! {
        #[allow(non_snake_case)]
        #[allow(clippy::unused_unit)]
        fn #extract_ident(node: Option<tree_sitter::Node>, source: &[u8]) -> #leaf_type {
            #(#leaf_stmts)*
            #leaf_expr
        }
    });
}

fn gen_struct_or_variant(
    path: String,
    fields: Fields,
    variant_ident: Option<Ident>,
    containing_type: Ident,
    out: &mut Vec<Item>,
) {
    fields.iter().enumerate().for_each(|(i, field)| {
        let ident_str = field
            .ident
            .as_ref()
            .map(|v| v.to_string())
            .unwrap_or(format!("{}", i));
        gen_field(
            format!("{}_{}", path.clone(), ident_str),
            field.clone(),
            out,
        );
    });

    let extract_ident = Ident::new(&format!("extract_{}", path), Span::call_site());

    let children_parsed = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let ident_str = field
                .ident
                .as_ref()
                .map(|v| v.to_string())
                .unwrap_or(format!("{}", i));
            let ident = Ident::new(
                &format!("extract_{}_{}", path.clone(), ident_str),
                Span::call_site(),
            );

            let (_, is_vec) = try_extract_inner_type(&field.ty, "Vec");

            let expr = if is_vec {
                syn::parse_quote! {
                    #ident(Some(node), source)
                }
            } else {
                syn::parse_quote! {
                    #ident(node.child_by_field_name(#ident_str), source)
                }
            };

            if field.ident.is_none() {
                ParamOrField::Param(expr)
            } else {
                let field_name = field.ident.as_ref().unwrap();
                ParamOrField::Field(FieldValue {
                    attrs: vec![],
                    member: Member::Named(field_name.clone()),
                    colon_token: Some(Token![:](Span::call_site())),
                    expr,
                })
            }
        })
        .collect::<Vec<ParamOrField>>();

    if let Some(variant_ident) = variant_ident {
        out.push(syn::parse_quote! {
            #[allow(non_snake_case)]
            fn #extract_ident(node: tree_sitter::Node, source: &[u8]) -> #containing_type {
                #containing_type::#variant_ident(
                    #(#children_parsed),*
                )
            }
        });
    } else {
        out.push(syn::parse_quote! {
            #[allow(non_snake_case)]
            fn #extract_ident(node: tree_sitter::Node, source: &[u8]) -> #containing_type {
                #containing_type {
                    #(#children_parsed),*
                }
            }
        });
    }
}

fn expand_grammar(input: ItemMod) -> ItemMod {
    let grammar_name = input
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

    let (brace, new_contents) = input.content.unwrap();

    let root_type = new_contents
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
        .expect("Each parser must have the root type annotated with `#[rust_sitter::language]`");

    let mut transformed: Vec<Item> = new_contents
        .iter()
        .cloned()
        .flat_map(|c| match c {
            Item::Enum(mut e) => {
                let mut impl_body = vec![];
                e.variants.iter().for_each(|v| {
                    gen_struct_or_variant(
                        format!("{}_{}", e.ident, v.ident),
                        v.fields.clone(),
                        Some(v.ident.clone()),
                        e.ident.clone(),
                        &mut impl_body,
                    )
                });

                let mut match_cases: Vec<Arm> = vec![];
                e.variants.iter().for_each(|v| {
                    let variant_path = format!("{}_{}", e.ident, v.ident);
                    let extract_ident =
                        Ident::new(&format!("extract_{}", variant_path), Span::call_site());
                    match_cases.push(syn::parse_quote! {
                        #variant_path => #extract_ident(node.child(0).unwrap(), source)
                    });
                });

                e.attrs.retain(|a| !is_sitter_attr(a));
                e.variants.iter_mut().for_each(|v| {
                    v.attrs.retain(|a| !is_sitter_attr(a));
                    v.fields.iter_mut().for_each(|f| {
                        f.attrs.retain(|a| !is_sitter_attr(a));
                    });
                });

                let enum_name = &e.ident;
                let extract_impl: Item = syn::parse_quote! {
                    impl rust_sitter::Extract for #enum_name {
                        #[allow(non_snake_case)]
                        fn extract(node: tree_sitter::Node, source: &[u8]) -> Self {
                            #(#impl_body)*

                            match node.child(0).unwrap().kind() {
                                #(#match_cases),*,
                                _ => panic!()
                            }
                        }
                    }
                };

                vec![Item::Enum(e), extract_impl]
            }

            Item::Struct(mut s) => {
                let mut impl_body = vec![];

                gen_struct_or_variant(
                    s.ident.to_string(),
                    s.fields.clone(),
                    None,
                    s.ident.clone(),
                    &mut impl_body,
                );

                s.attrs.retain(|a| !is_sitter_attr(a));
                s.fields.iter_mut().for_each(|f| {
                    f.attrs.retain(|a| !is_sitter_attr(a));
                });

                let struct_name = &s.ident;
                let extract_ident =
                    Ident::new(&format!("extract_{}", struct_name), Span::call_site());

                let extract_impl: Item = syn::parse_quote! {
                    impl rust_sitter::Extract for #struct_name {
                        #[allow(non_snake_case)]
                        fn extract(node: tree_sitter::Node, source: &[u8]) -> Self {
                            #(#impl_body)*
                            #extract_ident(node, source)
                        }
                    }
                };

                vec![Item::Struct(s), extract_impl]
            }

            _ => panic!(),
        })
        .collect();

    let tree_sitter_ident = Ident::new(&format!("tree_sitter_{}", grammar_name), Span::call_site());

    transformed.push(syn::parse_quote! {
        extern "C" {
            fn #tree_sitter_ident() -> tree_sitter::Language;
        }
    });

    transformed.push(syn::parse_quote! {
        fn language() -> tree_sitter::Language {
            unsafe { #tree_sitter_ident() }
        }
    });

    transformed.push(syn::parse_quote! {
        pub fn parse(input: &str) -> core::result::Result<#root_type, Vec<rust_sitter::errors::ParseError>> {
            let mut parser = tree_sitter::Parser::new();
            parser.set_language(language()).unwrap();
            let tree = parser.parse(input, None).unwrap();
            let root_node = tree.root_node();

            if root_node.has_error() {
                let mut errors = vec![];
                rust_sitter::errors::collect_parsing_errors(
                    &root_node,
                    input.as_bytes(),
                    &mut errors,
                );

                Err(errors)
            } else {
                use rust_sitter::Extract;
                Ok(#root_type::extract(root_node, input.as_bytes()))
            }
        }
    });

    let mut filtered_attrs = input.attrs;
    filtered_attrs.retain(|a| !is_sitter_attr(a));
    ItemMod {
        attrs: filtered_attrs,
        vis: input.vis,
        mod_token: input.mod_token,
        ident: input.ident,
        content: Some((brace, transformed)),
        semi: input.semi,
    }
}

#[proc_macro_attribute]
pub fn language(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn leaf(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn prec_left(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn delimited(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn repeat(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

/// Mark a module to be analyzed for a Tree Sitter grammar.
#[proc_macro_attribute]
pub fn grammar(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: AttributeArgs = parse_macro_input!(attr);
    let module: ItemMod = parse_macro_input!(input);
    let expanded: ItemMod = expand_grammar(syn::parse_quote! {
        #[rust_sitter::grammar[#(#attrs),*]]
        #module
    });
    proc_macro::TokenStream::from(expanded.to_token_stream())
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use quote::ToTokens;
    use syn::parse_quote;
    use tempfile::tempdir;

    use super::expand_grammar;

    fn rustfmt_code(code: &str) -> String {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("temp.rs");
        let mut file = File::create(file_path.clone()).unwrap();

        writeln!(file, "{}", code).unwrap();
        drop(file);

        Command::new("rustfmt")
            .arg(file_path.to_str().unwrap())
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut file = File::open(file_path).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        drop(file);
        dir.close().unwrap();
        data
    }

    #[test]
    fn enum_transformed_fields() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse::<i32>().unwrap())]
                            i32
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_recursive() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                            i32
                        ),
                        Neg(
                            #[rust_sitter::leaf(text = "-")]
                            (),
                            Box<Expression>
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_prec_left() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                            i32
                        ),
                        #[rust_sitter::prec_left(1)]
                        Sub(
                            Box<Expression>,
                            #[rust_sitter::leaf(text = "-")]
                            (),
                            Box<Expression>
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn struct_extra() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32,
                        ),
                    }

                    #[rust_sitter::extra]
                    struct Whitespace {
                        #[rust_sitter::leaf(pattern = r"\s")]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn grammar_unboxed_field() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub struct Language {
                        e: Expression,
                    }

                    pub enum Expression {
                        Number(
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
                            i32
                        ),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn struct_repeat() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub struct NumberList {
                        numbers: Vec<Number>,
                    }

                    pub struct Number {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: i32
                    }

                    #[rust_sitter::extra]
                    struct Whitespace {
                        #[rust_sitter::leaf(pattern = r"\s")]
                        _whitespace: (),
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn struct_optional() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub struct Language {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: Option<i32>,
                        t: Option<Number>,
                    }

                    pub struct Number {
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                        v: i32
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }
}
