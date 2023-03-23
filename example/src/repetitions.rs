#[rust_sitter::grammar("repetitions")]
pub mod grammar {
    use rust_sitter::Spanned;

    #[rust_sitter::language]
    #[derive(Debug)]
    #[allow(dead_code)]
    pub struct NumberList {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")]
            ()
        )]
        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
        numbers: Spanned<Vec<Spanned<i32>>>,
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

#[rust_sitter::grammar("repetitions_without_delim")]
pub mod grammar2 {
    use rust_sitter::Spanned;

    #[rust_sitter::language]
    #[derive(Debug)]
    #[allow(dead_code)]
    pub struct NumberList {
        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
        numbers: Spanned<Vec<Spanned<i32>>>,
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

#[rust_sitter::grammar("repetitions_optional_elem")]
pub mod grammar3 {
    use rust_sitter::Spanned;

    #[rust_sitter::language]
    #[derive(Debug)]
    #[allow(dead_code)]
    pub struct NumberList {
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")]
            ()
        )]
        #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
        numbers: Spanned<Vec<Spanned<Option<i32>>>>,
        #[rust_sitter::skip(123)]
        metadata: u32,
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
    fn repetitions_grammar() {
        insta::assert_debug_snapshot!(grammar::parse(""));
        insta::assert_debug_snapshot!(grammar::parse("1"));
        insta::assert_debug_snapshot!(grammar::parse("1, 2"));
    }

    #[test]
    fn repetitions_grammar2() {
        insta::assert_debug_snapshot!(grammar2::parse(""));
        insta::assert_debug_snapshot!(grammar2::parse("1"));
        insta::assert_debug_snapshot!(grammar2::parse("1 2"));
    }

    #[test]
    fn repetitions_grammar3() {
        insta::assert_debug_snapshot!(grammar3::parse(""));
        insta::assert_debug_snapshot!(grammar3::parse("1,"));
        insta::assert_debug_snapshot!(grammar3::parse("1, 2"));
        insta::assert_debug_snapshot!(grammar3::parse("1,, 2"));
        insta::assert_debug_snapshot!(grammar3::parse("1,, 2,"));
    }
}
