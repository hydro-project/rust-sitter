#[rust_sitter::grammar("handles")]
pub mod grammar {
    use rust_sitter::Handle;

    #[rust_sitter::arena]
    #[derive(Default, Debug)]
    pub struct Arena;

    #[rust_sitter::language]
    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression {
        Literal(rust_sitter::Handle<Literal>),
        #[rust_sitter::prec_left(1)]
        Sub(
            rust_sitter::Handle<Expression>,
            #[rust_sitter::leaf(text = "-")] (),
            Handle<Expression>,
        ),
        #[rust_sitter::prec_left(2)]
        Mul(
            Handle<Expression>,
            #[rust_sitter::leaf(text = "*")] (),
            rust_sitter::Handle<Expression>,
        ),
        Array(
            #[rust_sitter::leaf(text = "[")] (),
            #[rust_sitter::repeat]
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")]
                ()
            )]
            Vec<Handle<Expression>>,
            #[rust_sitter::leaf(text = "]")] (),
        ),
    }

    #[derive(PartialEq, Debug)]
    pub enum Literal {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        Float(#[rust_sitter::leaf(pattern = r"\d+\.\d*", transform = |v| v.parse().unwrap())] f32),
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
    use grammar::*;

    #[wasm_bindgen_test::wasm_bindgen_test]
    #[test]
    fn successful_parses() {
        let (node, arena) = grammar::parse("1").unwrap();
        assert!(
            matches!(node, Expression::Literal(lit) if matches!(arena[lit], Literal::Number(1)))
        );

        let (node, arena) = grammar::parse(" 1.1").unwrap();
        assert!(
            matches!(node, Expression::Literal(lit) if matches!(arena[lit], Literal::Float(1.1)))
        );

        let (node, arena) = grammar::parse("1 - 2").unwrap();
        assert!(matches!(node, Expression::Sub(lhs, _, rhs)
            if matches!(arena[lhs], Expression::Literal(lit) if matches!(arena[lit], Literal::Number(1)))
            && matches!(arena[rhs], Expression::Literal(lit) if matches!(arena[lit], Literal::Number(2)))
        ));

        let (node, arena) = grammar::parse("[1, 2, 3]").unwrap();
        assert!(matches!(node, Expression::Array(_, elements, _)
            if matches!(arena[elements[0]], Expression::Literal(lit) if matches!(arena[lit], Literal::Number(1)))
            && matches!(arena[elements[1]], Expression::Literal(lit) if matches!(arena[lit], Literal::Number(2)))
            && matches!(arena[elements[2]], Expression::Literal(lit) if matches!(arena[lit], Literal::Number(3)))
        ));
    }

    #[test]
    fn failed_parses() {
        insta::assert_debug_snapshot!(grammar::parse("1 + 2"));
        insta::assert_debug_snapshot!(grammar::parse("1 - 2 -"));
        insta::assert_debug_snapshot!(grammar::parse("a1"));
        insta::assert_debug_snapshot!(grammar::parse("1a"));
    }
}
