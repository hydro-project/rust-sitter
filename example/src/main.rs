#[rust_sitter::grammar]
pub mod grammar {
  #[rust_sitter::language]
  #[derive(Debug)]
  pub enum Expression {
    Number(
      #[rust_sitter::leaf(r"/\d+/", transform = |v: &str| v.parse::<i32>().unwrap())]
      i32
    ),
    Add(
      Box<Expression>,
      #[rust_sitter::leaf(r"+", transform = |v| ())]
      (),
      Box<Expression>
    ),
    Sub(
      Box<Expression>,
      #[rust_sitter::leaf(r"-", transform = |v| ())]
      (),
      Box<Expression>
    ),
  }
}

fn main() {
  let foo = grammar::parse("123");
  dbg!(foo);
}
