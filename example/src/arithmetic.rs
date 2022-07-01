#[rust_sitter::grammar("arithmetic")]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub enum Expression {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        #[rust_sitter::prec_left(1)]
        Sub(
            Box<Expression>,
            #[rust_sitter::leaf(text = "-")] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec_left(2)]
        Mul(
            Box<Expression>,
            #[rust_sitter::leaf(text = "*")] (),
            Box<Expression>,
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
    fn arithmetic_grammar() {
        // successful parses
        insta::assert_debug_snapshot!(grammar::parse("1"));
        insta::assert_debug_snapshot!(grammar::parse("1 - 2"));
        insta::assert_debug_snapshot!(grammar::parse("1 - 2 - 3"));
        insta::assert_debug_snapshot!(grammar::parse("1 - 2 * 3"));
        insta::assert_debug_snapshot!(grammar::parse("1 * 2 * 3"));
        insta::assert_debug_snapshot!(grammar::parse("1 * 2 - 3"));

        // failed parses
        insta::assert_debug_snapshot!(grammar::parse("1 + 2"));
        insta::assert_debug_snapshot!(grammar::parse("1 - 2 -"));
        insta::assert_debug_snapshot!(grammar::parse("a1"));
        insta::assert_debug_snapshot!(grammar::parse("1a"));
    }
}
