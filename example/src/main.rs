#[rust_sitter::grammar]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub enum Expression {
        Number(
            #[rust_sitter::leaf(pattern = r"\d+", transform = |v: &str| v.parse::<i32>().unwrap())]
            i32,
        ),
        #[rust_sitter::prec_left(1)]
        Sub(
            Box<Expression>,
            #[rust_sitter::leaf(text = "-", transform = |_v| ())] (),
            Box<Expression>,
        ),
    }
}

fn main() {
    dbg!(grammar::parse("123"));
    dbg!(grammar::parse("1-2"));
    dbg!(grammar::parse("1-2-3"));
}
