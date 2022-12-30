use std::collections::HashSet;

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

    let mut skip_over = HashSet::new();
    skip_over.insert("Spanned");
    skip_over.insert("Box");

    let (inner_type, is_vec) = try_extract_inner_type(&leaf_type, "Vec", &skip_over);
    let (inner_type, is_option) = try_extract_inner_type(&inner_type, "Option", &skip_over);

    let (leaf_stmts, leaf_expr): (Vec<Stmt>, Expr) = match transform_param {
        Some(closure) => {
            if is_vec {
                panic!("Vec or Spanned of leaves is not supported");
            }

            (
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
            )
        }
        None => {
            let element_field = format!("{}_vec_element", path);

            (
                vec![],
                syn::parse_quote!(rust_sitter::Extract::extract(node, source, Some(#element_field))),
            )
        }
    };

    out.push(syn::parse_quote! {
        #[allow(non_snake_case)]
        #[allow(clippy::unused_unit)]
        fn #extract_ident(node: Option<rust_sitter::Node>, source: &[u8]) -> #leaf_type {
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

    let mut have_named_field = false;

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

            let mut skip_over = HashSet::new();
            skip_over.insert("Spanned");
            skip_over.insert("Box");

            let expr = syn::parse_quote! {
                #ident(node.child_by_field_name(#ident_str), source)
            };

            if field.ident.is_none() {
                ParamOrField::Param(expr)
            } else {
                let field_name = field.ident.as_ref().unwrap();
                have_named_field = true;
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
        if have_named_field {
            out.push(syn::parse_quote! {
                #[allow(non_snake_case)]
                fn #extract_ident(node: rust_sitter::Node, source: &[u8]) -> #containing_type {
                    #containing_type::#variant_ident {
                        #(#children_parsed),*
                    }
                }
            });
        } else {
            out.push(syn::parse_quote! {
                #[allow(non_snake_case)]
                fn #extract_ident(node: rust_sitter::Node, source: &[u8]) -> #containing_type {
                    #containing_type::#variant_ident(
                        #(#children_parsed),*
                    )
                }
            });
        }
    } else {
        out.push(syn::parse_quote! {
            #[allow(non_snake_case)]
            fn #extract_ident(node: rust_sitter::Node, source: &[u8]) -> #containing_type {
                #containing_type {
                    #(#children_parsed),*
                }
            }
        });
    }
}

pub fn expand_grammar(input: ItemMod) -> ItemMod {
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

                let match_cases: Vec<Arm> = e
                    .variants
                    .iter()
                    .map(|v| {
                        let variant_path = format!("{}_{}", e.ident, v.ident);
                        let extract_ident =
                            Ident::new(&format!("extract_{}", variant_path), Span::call_site());
                        syn::parse_quote! {
                            #variant_path => #extract_ident(node.child(0).unwrap(), source)
                        }
                    })
                    .collect();

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
                        fn extract(node: Option<rust_sitter::Node>, source: &[u8], _vec_field_name: Option<&str>) -> Self {
                            let node = node.unwrap();
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
                        fn extract(node: Option<rust_sitter::Node>, source: &[u8], _vec_field_name: Option<&str>) -> Self {
                            let node = node.unwrap();
                            #(#impl_body)*
                            #extract_ident(node, source)
                        }
                    }
                };

                vec![Item::Struct(s), extract_impl]
            }

            o => vec![o],
        })
        .collect();

    let tree_sitter_ident = Ident::new(&format!("tree_sitter_{}", grammar_name), Span::call_site());

    transformed.push(syn::parse_quote! {
        extern "C" {
            fn #tree_sitter_ident() -> rust_sitter::Language;
        }
    });

    transformed.push(syn::parse_quote! {
        fn language() -> rust_sitter::Language {
            unsafe { #tree_sitter_ident() }
        }
    });

    transformed.push(syn::parse_quote! {
      pub fn parse(input: &str) -> core::result::Result<#root_type, Vec<rust_sitter::errors::ParseError>> {
          let mut parser = rust_sitter::Parser::new();
          parser.set_language(language()).unwrap();
          let tree = parser.parse(input, None).unwrap();
          let root_node = tree.root_node();

          if root_node.has_error() {
              let mut errors = vec![];
              rust_sitter::errors::collect_parsing_errors(
                  &root_node,
                  &mut errors,
              );

              Err(errors)
          } else {
              use rust_sitter::Extract;
              Ok(rust_sitter::Extract::extract(Some(root_node), input.as_bytes(), None))
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
