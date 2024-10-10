use std::collections::HashSet;

use crate::errors::IteratorExt as _;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use rust_sitter_common::*;
use syn::{parse::Parse, punctuated::Punctuated, *};

fn is_sitter_attr(attr: &Attribute) -> bool {
    attr.path()
        .segments
        .iter()
        .next()
        .map(|segment| segment.ident == "rust_sitter")
        .unwrap_or(false)
}

pub enum ParamOrField {
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

fn gen_field(ident_str: String, leaf: Field) -> Expr {
    let leaf_type = leaf.ty;

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| attr.path() == &syn::parse_quote!(rust_sitter::leaf));

    let leaf_params = leaf_attr.and_then(|a| {
        a.parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
            .ok()
    });

    let transform_param = leaf_params.as_ref().and_then(|p| {
        p.iter()
            .find(|param| param.path == "transform")
            .map(|p| p.expr.clone())
    });

    let (leaf_type, closure_expr): (Type, Expr) = match transform_param {
        Some(closure) => {
            let mut non_leaf = HashSet::new();
            non_leaf.insert("Spanned");
            non_leaf.insert("Box");
            non_leaf.insert("Option");
            non_leaf.insert("Vec");
            let wrapped_leaf_type = wrap_leaf_type(&leaf_type, &non_leaf);
            (wrapped_leaf_type, syn::parse_quote!(Some(&#closure)))
        }
        None => (leaf_type, syn::parse_quote!(None)),
    };

    syn::parse_quote!({
        ::rust_sitter::__private::extract_field::<#leaf_type,_>(cursor, source, last_idx, #ident_str, #closure_expr)
    })
}

fn gen_struct_or_variant(
    fields: Fields,
    variant_ident: Option<Ident>,
    containing_type: Ident,
    container_attrs: Vec<Attribute>,
) -> Result<Expr> {
    let children_parsed = if fields == Fields::Unit {
        let expr = {
            let dummy_field = Field {
                attrs: container_attrs,
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: None,
                colon_token: None,
                ty: Type::Verbatim(quote!(())), // unit type.
            };

            gen_field("unit".to_string(), dummy_field)
        };
        vec![ParamOrField::Param(expr)]
    } else {
        fields
            .iter()
            .enumerate()
            .map(|(i, field)| {
                let expr = if let Some(skip_attrs) = field
                    .attrs
                    .iter()
                    .find(|attr| attr.path() == &syn::parse_quote!(rust_sitter::skip))
                {
                    skip_attrs.parse_args::<syn::Expr>()?
                } else {
                    let ident_str = field
                        .ident
                        .as_ref()
                        .map(|v| v.to_string())
                        .unwrap_or(format!("{i}"));

                    gen_field(ident_str, field.clone())
                };

                let field = if let Some(field_name) = &field.ident {
                    ParamOrField::Field(FieldValue {
                        attrs: vec![],
                        member: Member::Named(field_name.clone()),
                        colon_token: Some(Token![:](Span::call_site())),
                        expr,
                    })
                } else {
                    ParamOrField::Param(expr)
                };
                Ok(field)
            })
            .sift::<Vec<ParamOrField>>()?
    };

    let construct_name = match variant_ident {
        Some(ident) => quote! {
            #containing_type::#ident
        },
        None => quote! {
            #containing_type
        },
    };

    let construct_expr = {
        match &fields {
            Fields::Unit => {
                let ParamOrField::Param(ref expr) = children_parsed[0] else {
                    unreachable!()
                };

                quote! {
                    {
                        #expr;
                        #construct_name
                    }
                }
            }
            Fields::Named(_) => quote! {
                #construct_name {
                    #(#children_parsed),*
                }
            },
            Fields::Unnamed(_) => quote! {
                #construct_name(
                    #(#children_parsed),*
                )
            },
        }
    };

    Ok(
        syn::parse_quote!(::rust_sitter::__private::extract_struct_or_variant(node, move |cursor, last_idx| #construct_expr)),
    )
}

pub fn expand_grammar(input: ItemMod) -> Result<ItemMod> {
    let grammar_name = input
        .attrs
        .iter()
        .find_map(|a| {
            if a.path() == &syn::parse_quote!(rust_sitter::grammar) {
                let grammar_name_expr = a.parse_args_with(Expr::parse).ok();
                if let Some(Expr::Lit(ExprLit {
                    attrs: _,
                    lit: Lit::Str(s),
                })) = grammar_name_expr
                {
                    Some(Ok(s.value()))
                } else {
                    Some(Err(syn::Error::new(
                        Span::call_site(),
                        "Expected a string literal grammar name",
                    )))
                }
            } else {
                None
            }
        })
        .transpose()?
        .ok_or_else(|| syn::Error::new(Span::call_site(), "Each grammar must have a name"))?;

    let (brace, new_contents) = input.content.ok_or_else(|| {
        syn::Error::new(
            Span::call_site(),
            "Expected the module to have inline contents (`mod my_module { .. }` syntax)",
        )
    })?;

    let root_type = new_contents
        .iter()
        .find_map(|item| match item {
            Item::Enum(ItemEnum { ident, attrs, .. })
            | Item::Struct(ItemStruct { ident, attrs, .. }) => {
                if attrs
                    .iter()
                    .any(|attr| attr.path() == &syn::parse_quote!(rust_sitter::language))
                {
                    Some(ident.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
        .ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "Each parser must have the root type annotated with `#[rust_sitter::language]`",
            )
        })?;

    let mut transformed: Vec<Item> = new_contents
        .iter()
        .cloned()
        .map(|c| match c {
            Item::Enum(mut e) => {
                    let match_cases: Vec<Arm> = e.variants.iter().map(|v| {
                        let variant_path = format!("{}_{}", e.ident, v.ident);

                        let extract_expr = gen_struct_or_variant(
                            v.fields.clone(),
                            Some(v.ident.clone()),
                            e.ident.clone(),
                            v.attrs.clone(),
                        )?;
                        Ok(syn::parse_quote! {
                            #variant_path => return #extract_expr
                        })
                    }).sift::<Vec<Arm>>()?;

                    e.attrs.retain(|a| !is_sitter_attr(a));
                    e.variants.iter_mut().for_each(|v| {
                        v.attrs.retain(|a| !is_sitter_attr(a));
                        v.fields.iter_mut().for_each(|f| {
                            f.attrs.retain(|a| !is_sitter_attr(a));
                        });
                    });

                    let enum_name = &e.ident;
                    let extract_impl: Item = syn::parse_quote! {
                        impl ::rust_sitter::Extract<#enum_name> for #enum_name {
                            type LeafFn = ();

                            #[allow(non_snake_case)]
                            fn extract(node: Option<::rust_sitter::tree_sitter::Node>, source: &[u8], _last_idx: usize, _leaf_fn: Option<&Self::LeafFn>) -> Self {
                                let node = node.unwrap();

                                let mut cursor = node.walk();
                                assert!(cursor.goto_first_child(), "Could not find a child corresponding to any enum branch");
                                loop {
                                    let node = cursor.node();
                                    match node.kind() {
                                        #(#match_cases),*,
                                        _ => if !cursor.goto_next_sibling() {
                                            panic!("Could not find a child corresponding to any enum branch")
                                        }
                                    }
                                }
                            }
                        }
                    };
                    Ok(vec![Item::Enum(e), extract_impl])
            }

            Item::Struct(mut s) => {
                    let struct_name = &s.ident;
                    let extract_expr = gen_struct_or_variant(
                        s.fields.clone(),
                        None,
                        s.ident.clone(),
                        s.attrs.clone(),
                    )?;

                    s.attrs.retain(|a| !is_sitter_attr(a));
                    s.fields.iter_mut().for_each(|f| {
                        f.attrs.retain(|a| !is_sitter_attr(a));
                    });


                    let extract_impl: Item = syn::parse_quote! {
                        impl ::rust_sitter::Extract<#struct_name> for #struct_name {
                            type LeafFn = ();

                            #[allow(non_snake_case)]
                            fn extract(node: Option<::rust_sitter::tree_sitter::Node>, source: &[u8], last_idx: usize, _leaf_fn: Option<&Self::LeafFn>) -> Self {
                                let node = node.unwrap();
                                #extract_expr
                            }
                        }
                    };

                    Ok(vec![Item::Struct(s), extract_impl])
            }

            o => Ok(vec![o]),
        })
        .sift::<Vec<_>>()?.into_iter().flatten().collect();

    let tree_sitter_ident = Ident::new(&format!("tree_sitter_{grammar_name}"), Span::call_site());

    transformed.push(syn::parse_quote! {
        extern "C" {
            fn #tree_sitter_ident() -> ::rust_sitter::tree_sitter::Language;
        }
    });

    transformed.push(syn::parse_quote! {
        pub fn language() -> ::rust_sitter::tree_sitter::Language {
            unsafe { #tree_sitter_ident() }
        }
    });

    let root_type_docstr = format!("[`{root_type}`]");
    transformed.push(syn::parse_quote! {
    /// Parse an input string according to the grammar. Returns either any parsing errors that happened, or a
    #[doc = #root_type_docstr]
    /// instance containing the parsed structured data.
      pub fn parse(input: &str) -> core::result::Result<#root_type, Vec<::rust_sitter::errors::ParseError>> {
        ::rust_sitter::__private::parse::<#root_type>(input, language)
      }
  });

    let mut filtered_attrs = input.attrs;
    filtered_attrs.retain(|a| !is_sitter_attr(a));
    Ok(ItemMod {
        attrs: filtered_attrs,
        vis: input.vis,
        unsafety: None,
        mod_token: input.mod_token,
        ident: input.ident,
        content: Some((brace, transformed)),
        semi: input.semi,
    })
}
