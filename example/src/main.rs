use std::io::Write;

#[rust_sitter::grammar]
pub mod arithmetic_grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub enum Expression {
        Number(
            #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse().unwrap())] i32,
        ),
        #[rust_sitter::prec_left(1)]
        Sub(
            Box<Expression>,
            #[rust_sitter::leaf(text = "-", transform = |_v| ())] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec_left(2)]
        Mul(
            Box<Expression>,
            #[rust_sitter::leaf(text = "*", transform = |_v| ())] (),
            Box<Expression>,
        ),
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s", transform = |_v| ())]
        _whitespace: (),
    }
}

fn main() {
    let stdin = std::io::stdin();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        stdin.read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            break;
        }

        println!("{:?}", arithmetic_grammar::parse(input));
    }
}

#[cfg(test)]
mod tests {
    use super::arithmetic_grammar;

    #[test]
    fn arithmetic_grammar() {
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2 - 3"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 - 2 * 3"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 * 2 * 3"));
        insta::assert_debug_snapshot!(arithmetic_grammar::parse("1 * 2 - 3"));
    }
}
