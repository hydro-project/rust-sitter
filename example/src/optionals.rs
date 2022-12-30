#[rust_sitter::grammar("optionals")]
#[allow(dead_code)]
mod grammar {
    use rust_sitter::Spanned;

    #[rust_sitter::language]
    #[derive(Debug)]
    pub struct Language {
        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
        v: Option<i32>,
        #[rust_sitter::leaf(text = "_")]
        _s: (),
        t: Spanned<Option<Number>>,
        #[rust_sitter::leaf(text = ".")]
        _d: Option<()>,
    }

    #[derive(Debug)]
    pub struct Number {
        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
        v: i32,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn optional_grammar() {
        insta::assert_debug_snapshot!(grammar::parse("_"));
        insta::assert_debug_snapshot!(grammar::parse("_."));
        insta::assert_debug_snapshot!(grammar::parse("1_"));
        insta::assert_debug_snapshot!(grammar::parse("1_."));
        insta::assert_debug_snapshot!(grammar::parse("1_2"));
        insta::assert_debug_snapshot!(grammar::parse("1_2."));
        insta::assert_debug_snapshot!(grammar::parse("_2"));
        insta::assert_debug_snapshot!(grammar::parse("_2."));
    }
}
