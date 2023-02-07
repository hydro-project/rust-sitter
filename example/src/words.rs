#[rust_sitter::grammar("words")]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub struct Words {
        #[rust_sitter::leaf(text = r"if")]
        _keyword: (),
        #[rust_sitter::word]
        #[rust_sitter::leaf(pattern = r"[a-z_]+", transform = |v| v.to_string())]
        _word: String,
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
