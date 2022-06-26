use syn::*;
use quote::ToTokens;

fn is_sitter_attr(attr: &Attribute) -> bool {
    let ident = &attr.path.segments.iter().next().unwrap().ident;
    ident == "rust_sitter"
}

fn expand_grammar(input: ItemMod) -> ItemMod {
    let (brace, new_contents) = input.content.unwrap();
    let mut transformed: Vec<Item> = new_contents.iter().cloned().flat_map(|c| match c {
        Item::Enum(mut e) => {
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
                    impl #enum_name {
                        fn extract_Number(node: tree_sitter::Node, source: &[u8]) -> Self {
                            Self::Number((|v: &str| v.parse::<i32>().unwrap())(node.utf8_text(source).unwrap()))
                        }

                        fn extract_Expression(node: tree_sitter::Node, source: &[u8]) -> Self {
                            match node.child(0).unwrap().kind() {
                                "Expression_Number" => Self::extract_Number(node.child(0).unwrap(), source),
                                _ => panic!()
                            }
                        }
                    }
                }
            ]
        }

        _ => panic!()
    }).collect();

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

            Expression::extract_Expression(root_node.child(0).unwrap(), input.as_bytes())
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
pub fn language(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn leaf(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
