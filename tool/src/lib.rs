use syn::{parse_quote, Item};

mod expansion;
use expansion::*;

/// Generates JSON strings defining Tree Sitter grammars for every Rust Sitter
/// grammar found in the given module and recursive submodules.
pub fn generate_grammars(root_file: &Path) -> Vec<String> {
    let root_file = syn_inline_mod::parse_and_inline_modules(root_file).items;
    let mut out = vec![];
    root_file
        .iter()
        .for_each(|i| generate_all_grammars(i, &mut out));
    out
}

fn generate_all_grammars(item: &Item, out: &mut Vec<String>) {
    if let Item::Mod(m) = item {
        m.content
            .iter()
            .for_each(|(_, items)| items.iter().for_each(|i| generate_all_grammars(i, out)));

        if m.attrs
            .iter()
            .any(|a| a.path == parse_quote!(rust_sitter::grammar))
        {
            out.push(generate_grammar(m).to_string())
        }
    }
}

#[cfg(feature = "build_parsers")]
use std::io::Write;
use std::path::Path;

#[cfg(feature = "build_parsers")]
use tree_sitter_cli::generate;

#[cfg(feature = "build_parsers")]
/// Using the `cc` crate, generates and compiles a C parser with Tree Sitter
/// for every Rust Sitter grammar found in the given module and recursive
/// submodules.
pub fn build_parsers(root_file: &Path) {
    use std::env;

    generate_grammars(root_file).iter().for_each(|grammar| {
        let dir = tempfile::Builder::new()
            .prefix("grammar")
            .tempdir()
            .unwrap();
        let grammar_file = dir.path().join("parser.c");
        let mut f = std::fs::File::create(grammar_file).unwrap();

        let (grammar_name, grammar_c) = generate::generate_parser_for_grammar(grammar).unwrap();
        f.write_all(grammar_c.as_bytes()).unwrap();
        drop(f);

        let header_dir = dir.path().join("tree_sitter");
        std::fs::create_dir(&header_dir).unwrap();
        let mut parser_file = std::fs::File::create(header_dir.join("parser.h")).unwrap();
        parser_file
            .write_all(tree_sitter::PARSER_HEADER.as_bytes())
            .unwrap();
        drop(parser_file);

        let sysroot_dir = dir.path().join("sysroot");
        if env::var("TARGET").unwrap() == "wasm32-unknown-unknown" {
            std::fs::create_dir(&sysroot_dir).unwrap();
            let mut stdint = std::fs::File::create(sysroot_dir.join("stdint.h")).unwrap();
            stdint
                .write_all(include_bytes!("wasm-sysroot/stdint.h"))
                .unwrap();
            drop(stdint);

            let mut stdlib = std::fs::File::create(sysroot_dir.join("stdlib.h")).unwrap();
            stdlib
                .write_all(include_bytes!("wasm-sysroot/stdlib.h"))
                .unwrap();
            drop(stdlib);
        }

        cc::Build::new()
            .include(&dir)
            .include(&sysroot_dir)
            .flag("-Wno-unused-label")
            .flag("-Wno-unused-but-set-variable")
            .flag("-Wno-unknown-warning-option")
            .file(dir.path().join("parser.c"))
            .compile(&grammar_name);
    });
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::generate_grammar;

    #[test]
    fn enum_with_named_field() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
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
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn enum_transformed_fields() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            mod grammar {
                #[rust_sitter::language]
                pub enum Expression {
                    Number(
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
                        i32
                    ),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn enum_recursive() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            mod grammar {
                #[rust_sitter::language]
                pub enum Expression {
                    Number(
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
                        i32
                    ),
                    Neg(
                        #[rust_sitter::leaf(text = "-", transform = |v| ())]
                        (),
                        Box<Expression>
                    ),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn enum_prec_left() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            mod grammar {
                #[rust_sitter::language]
                pub enum Expression {
                    Number(
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
                        i32
                    ),
                    #[rust_sitter::prec_left(1)]
                    Sub(
                        Box<Expression>,
                        #[rust_sitter::leaf(text = "-", transform = |v| ())]
                        (),
                        Box<Expression>
                    ),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn grammar_with_extras() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            mod grammar {
                #[rust_sitter::language]
                pub enum Expression {
                    Number(
                        #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
                        i32
                    ),
                }

                #[rust_sitter::extra]
                struct Whitespace {
                    #[rust_sitter::leaf(pattern = r"\s", transform = |_v| ())]
                    _whitespace: (),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn grammar_unboxed_field() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
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
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn grammar_repeat() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            pub mod grammar {
                #[rust_sitter::language]
                pub struct NumberList {
                    #[rust_sitter::delimited(
                        #[rust_sitter::leaf(text = ",")]
                        ()
                    )]
                    numbers: Vec<Number>,
                }

                pub struct Number {
                    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                    v: i32,
                }

                #[rust_sitter::extra]
                struct Whitespace {
                    #[rust_sitter::leaf(pattern = r"\s")]
                    _whitespace: (),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn grammar_repeat_no_delimiter() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            pub mod grammar {
                #[rust_sitter::language]
                pub struct NumberList {
                    numbers: Vec<Number>,
                }

                pub struct Number {
                    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                    v: i32,
                }

                #[rust_sitter::extra]
                struct Whitespace {
                    #[rust_sitter::leaf(pattern = r"\s")]
                    _whitespace: (),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn grammar_repeat1() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            pub mod grammar {
                #[rust_sitter::language]
                pub struct NumberList {
                    #[rust_sitter::repeat(non_empty = true)]
                    #[rust_sitter::delimited(
                        #[rust_sitter::leaf(text = ",")]
                        ()
                    )]
                    numbers: Vec<Number>,
                }

                pub struct Number {
                    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                    v: i32,
                }

                #[rust_sitter::extra]
                struct Whitespace {
                    #[rust_sitter::leaf(pattern = r"\s")]
                    _whitespace: (),
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn struct_optional() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
            #[rust_sitter::grammar("test")]
            mod grammar {
                #[rust_sitter::language]
                pub struct Language {
                    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                    v: Option<i32>,
                    #[rust_sitter::leaf(pattern = r" ", transform = |v| ())]
                    space: (),
                    t: Option<Number>,
                }

                pub struct Number {
                    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
                    v: i32
                }
            }
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn enum_with_unamed_vector() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
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
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }

    #[test]
    fn spanned_in_vec() {
        let m = if let syn::Item::Mod(m) = parse_quote! {
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
        } {
            m
        } else {
            panic!()
        };

        let grammar = generate_grammar(&m);
        insta::assert_display_snapshot!(grammar);
        tree_sitter_cli::generate::generate_parser_for_grammar(&grammar.to_string()).unwrap();
    }
}
