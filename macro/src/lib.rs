use quote::ToTokens;
use spanned::Spanned;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

fn is_sitter_attr(attr: &Attribute) -> bool {
    let ident = &attr.path.segments.iter().next().unwrap().ident;
    ident == "rust_sitter"
}

#[derive(Debug, Clone, PartialEq)]
struct NameValueExpr {
    pub path: Ident,
    pub eq_token: Token![=],
    pub expr: Expr,
}

impl Parse for NameValueExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(NameValueExpr {
            path: input.parse()?,
            eq_token: input.parse()?,
            expr: input.parse()?,
        })
    }
}

fn gen_leaf(path: String, leaf: Field, out: &mut Vec<Item>) {
    let extract_ident = Ident::new(&format!("extract_{}", path), leaf.span());
    let leaf_type = leaf.ty;

    let leaf_text_expr: Expr = syn::parse_quote! {
        node.utf8_text(source).unwrap()
    };

    let leaf_attr = leaf
        .attrs
        .iter()
        .find(|attr| attr.path == syn::parse_quote!(rust_sitter::leaf))
        .unwrap();

    let leaf_params = leaf_attr
        .parse_args_with(Punctuated::<NameValueExpr, Token![,]>::parse_terminated)
        .unwrap();

    let transform_param = leaf_params.iter().find(|param| param.path == "transform");
    let leaf_expr = match transform_param {
        Some(t) => {
            let closure = &t.expr;
            syn::parse_quote! {
                (#closure)(#leaf_text_expr)
            }
        }
        None => leaf_text_expr,
    };

    out.push(syn::parse_quote! {
        #[allow(non_snake_case)]
        fn #extract_ident(node: tree_sitter::Node, source: &[u8]) -> #leaf_type {
            #leaf_expr
        }
    });
}

fn gen_enum_variant(path: String, variant: Variant, containing_type: Ident, out: &mut Vec<Item>) {
    variant.fields.iter().enumerate().for_each(|(i, field)| {
        let ident_str = field
            .ident
            .as_ref()
            .map(|v| v.to_string())
            .unwrap_or(format!("{}", i));
        gen_leaf(
            format!("{}_{}", path.clone(), ident_str),
            field.clone(),
            out,
        );
    });

    let variant_ident = &variant.ident;
    let extract_ident = Ident::new(&format!("extract_{}", path), variant.span());

    let children_parsed = variant
        .fields
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
                field.span(),
            );
            syn::parse_quote! {
                #ident(node.child(#i).unwrap(), source)
            }
        })
        .collect::<Vec<Expr>>();

    out.push(syn::parse_quote! {
        #[allow(non_snake_case)]
        fn #extract_ident(node: tree_sitter::Node, source: &[u8]) -> #containing_type {
            #containing_type::#variant_ident(
                #(#children_parsed),*
            )
        }
    });
}

fn expand_grammar(input: ItemMod) -> ItemMod {
    let (brace, new_contents) = input.content.unwrap();
    let mut transformed: Vec<Item> = new_contents
        .iter()
        .cloned()
        .flat_map(|c| match c {
            Item::Enum(mut e) => {
                let mut impl_body = vec![];
                e.variants.iter().for_each(|v| {
                    gen_enum_variant(
                        format!("{}_{}", e.ident, v.ident),
                        v.clone(),
                        e.ident.clone(),
                        &mut impl_body,
                    )
                });

                let mut match_cases: Vec<Arm> = vec![];
                e.variants.iter().for_each(|v| {
                    let variant_path = format!("{}_{}", e.ident, v.ident);
                    let extract_ident = Ident::new(&format!("extract_{}", variant_path), v.span());
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

                vec![
                    syn::parse_quote! {
                        #e
                    },
                    syn::parse_quote! {
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
                    },
                ]
            }

            _ => panic!(),
        })
        .collect();

    transformed.push(syn::parse_quote! {
        extern "C" {
            fn tree_sitter_grammar() -> tree_sitter::Language;
        }
    });

    transformed.push(syn::parse_quote! {
        fn language() -> tree_sitter::Language {
            unsafe { tree_sitter_grammar() }
        }
    });

    transformed.push(syn::parse_quote! {
        pub fn parse(input: &str) -> Expression {
            let mut parser = tree_sitter::Parser::new();
            parser.set_language(language()).unwrap();
            let tree = parser.parse(input, None).unwrap();
            let root_node = tree.root_node();

            use rust_sitter::Extract;
            Expression::extract(root_node.child(0).unwrap(), input.as_bytes())
        }
    });

    ItemMod {
        attrs: input.attrs,
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

/// Mark a module to be analyzed for a Tree Sitter grammar.
#[proc_macro_attribute]
pub fn grammar(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let expanded: ItemMod = expand_grammar(parse_macro_input!(input));
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
                mod ffi {
                    #[rust_sitter::language]
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
}
