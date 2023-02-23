use quote::ToTokens;
use syn::{parse_macro_input, AttributeArgs, ItemMod};

mod expansion;
use expansion::*;

#[proc_macro_attribute]
/// Marks the top level AST node where parsing should start.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::language]
/// pub struct Code {
///     ...
/// }
/// ```
pub fn language(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Marks the AST node as an extra, which can be safely skipped over while parsing.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::extra]
/// pub struct Comment {
///     ...
/// }
/// ```
pub fn extra(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a field which matches a specific token in the source string.
/// The token can be defined by passing one of two arguments
/// - `text`: a string literal that will be exactly matched
/// - `pattern`: a regular expression that will be matched against the source string
///
/// If the resulting token needs to be converted into a richer type at runtime,
/// such as a number, then the `transform` argument can be used to specify a function
/// that will be called with the token's text.
///
/// ## Example
/// ```ignore
/// Number(
///     #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
///     u32
/// )
/// ```
pub fn leaf(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a field that does not correspond to anything in the input string,
/// such as some metadata. Takes a single, unnamed argument, which is the value
/// used to populate the field at runtime.
///
/// ## Example
/// ```ignore
/// struct MyNode {
///    ...,
///    #[rust_sitter::skip(false)]
///    node_visited: bool
/// }
/// ```
pub fn skip(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a precedence level for a non-terminal that has no associativity.
///
/// This annotation takes a single, unnamed parameter, which specifies the precedence level.
/// This is used to resolve conflicts with other non-terminals, so that the one with the higher
/// precedence will bind more tightly (appear lower in the parse tree).
///
/// ## Example
/// ```ignore
/// #[rust_sitter::prec(1)]
/// PriorityExpr(Box<Expr>, Box<Expr>)
/// ```
pub fn prec(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a precedence level for a non-terminal that should be left-associative.
/// For example, with subtraction we expect 1 - 2 - 3 to be parsed as (1 - 2) - 3,
/// which corresponds to a left-associativity.
///
/// This annotation takes a single, unnamed parameter, which specifies the precedence level.
/// This is used to resolve conflicts with other non-terminals, so that the one with the higher
/// precedence will bind more tightly (appear lower in the parse tree).
///
/// ## Example
/// ```ignore
/// #[rust_sitter::prec_left(1)]
/// Subtract(Box<Expr>, Box<Expr>)
/// ```
pub fn prec_left(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// Defines a precedence level for a non-terminal that should be right-associative.
/// For example, with cons we could have 1 :: 2 :: 3 to be parsed as 1 :: (2 :: 3),
/// which corresponds to a right-associativity.
///
/// This annotation takes a single, unnamed parameter, which specifies the precedence level.
/// This is used to resolve conflicts with other non-terminals, so that the one with the higher
/// precedence will bind more tightly (appear lower in the parse tree).
///
/// ## Example
/// ```ignore
/// #[rust_sitter::prec_right(1)]
/// Cons(Box<Expr>, Box<Expr>)
/// ```
pub fn prec_right(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// On `Vec<_>` typed fields, specifies a non-terminal that should be parsed in between the elements.
/// The [`rust_sitter::repeat`] annotation must be used on the field as well.
///
/// This annotation takes a single, unnamed argument, which specifies a field type to parse. This can
/// either be a reference to another type, or can be defined as a `leaf` field. Generally, the argument
/// is parsed using the same rules as an unnamed field of an enum variant.
///
/// ## Example
/// ```ignore
/// #[rust_sitter::delimited(
///     #[rust_sitter::leaf(text = ",")]
///     ()
/// )]
/// numbers: Vec<Number>
/// ```
pub fn delimited(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
/// On `Vec<_>` typed fields, specifies additional config for how the repeated elements should
/// be parsed. In particular, this annotation takes the following named arguments:
/// - `non_empty` - if this argument is `true`, then there must be at least one element parsed
///
/// ## Example
/// ```ignore
/// #[rust_sitter::repeat(non_empty = true)]
/// numbers: Vec<Number>
/// ```
pub fn repeat(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

/// Mark a module to be analyzed for a Rust Sitter grammar. Takes a single, unnamed argument, which
/// specifies the name of the grammar. This name must be unique across all Rust Sitter grammars within
/// a compilation unit.
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

        writeln!(file, "{code}").unwrap();
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

    #[test]
    fn enum_with_unamed_vector() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    pub struct Number {
                            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                            value: u32
                    }

                    #[rust_sitter::language]
                    pub enum Expr {
                        Numbers(
                            #[rust_sitter::repeat(non_empty = true)]
                            Vec<Number>
                        )
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn enum_with_named_field() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    #[rust_sitter::language]
                    pub enum Expr {
                        Number(
                                #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                                u32
                        ),
                        Neg {
                            #[rust_sitter::leaf(text = "!")]
                            _bang: (),
                            value: Box<Expr>,
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn spanned_in_vec() {
        insta::assert_display_snapshot!(rustfmt_code(
            &expand_grammar(parse_quote! {
                #[rust_sitter::grammar("test")]
                mod grammar {
                    use rust_sitter::Spanned;

                    #[rust_sitter::language]
                    pub struct NumberList {
                        numbers: Vec<Spanned<Number>>,
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
}
