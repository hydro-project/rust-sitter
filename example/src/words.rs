#[rust_sitter::grammar("words")]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub struct Words {
        #[rust_sitter::repeat]
        _words: Vec<Word>,
    }

    #[derive(Debug)]
    enum Word {
        Keyword(
            #[rust_sitter::leaf(text = r"if")]
            #[rust_sitter::word]
            (),
        ),
        Ident(
            #[rust_sitter::leaf(pattern = r"[a-zA-Z_][a-zA-Z0-9_]*", transform = |v| v.to_string())]
             String,
        ),
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn words_grammar() {
        insta::assert_debug_snapshot!(grammar::parse("if"));
        insta::assert_debug_snapshot!(grammar::parse("hello"));
        insta::assert_debug_snapshot!(grammar::parse("ifhello"));
        insta::assert_debug_snapshot!(grammar::parse("if hello"));
    }
}
