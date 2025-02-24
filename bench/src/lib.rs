#[rust_sitter::grammar("arithmetic")]
pub mod box_grammar {
    #[rust_sitter::language]
    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        #[rust_sitter::prec_left(0)]
        Sub(
            Box<Expression>,
            #[rust_sitter::leaf(text = "-")] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec_left(1)]
        Add(
            Box<Expression>,
            #[rust_sitter::leaf(text = "+")] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec_left(2)]
        Mul(
            Box<Expression>,
            #[rust_sitter::leaf(text = "*")] (),
            Box<Expression>,
        ),
        #[rust_sitter::prec_left(3)]
        Div(
            Box<Expression>,
            #[rust_sitter::leaf(text = "/")] (),
            Box<Expression>,
        ),
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}

#[rust_sitter::grammar("arithmetic_handles")]
pub mod handle_grammar {
    use rust_sitter::Handle;

    #[rust_sitter::arena]
    #[derive(Default, Debug)]
    pub struct Arena;

    #[rust_sitter::language]
    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        #[rust_sitter::prec_left(0)]
        Sub(
            Handle<Expression>,
            #[rust_sitter::leaf(text = "-")] (),
            Handle<Expression>,
        ),
        #[rust_sitter::prec_left(1)]
        Add(
            Handle<Expression>,
            #[rust_sitter::leaf(text = "+")] (),
            Handle<Expression>,
        ),
        #[rust_sitter::prec_left(2)]
        Mul(
            Handle<Expression>,
            #[rust_sitter::leaf(text = "*")] (),
            Handle<Expression>,
        ),
        #[rust_sitter::prec_left(3)]
        Div(
            Handle<Expression>,
            #[rust_sitter::leaf(text = "/")] (),
            Handle<Expression>,
        ),
    }

    #[rust_sitter::extra]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"\s")]
        _whitespace: (),
    }
}
