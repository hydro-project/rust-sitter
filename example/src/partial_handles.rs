#[rust_sitter::grammar("partial_arithmetic")]
pub mod grammar {
    use rust_sitter::Handle;

    #[rust_sitter::arena]
    #[derive(Debug, Default)]
    pub struct PartialArena;

    #[rust_sitter::language]
    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        #[rust_sitter::prec_left(1)]
        Sub(
            Handle<Expression>,
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
    use grammar::Expression;

    #[wasm_bindgen_test::wasm_bindgen_test]
    #[test]
    fn successful_parses() {
        let (node, _arena) = grammar::parse("1").unwrap();
        assert_eq!(node, Expression::Number(1));

        let (node, _arena) = grammar::parse(" 1").unwrap();
        assert_eq!(node, Expression::Number(1));

        let (node, arena) = grammar::parse("1 - 2").unwrap();
        assert!(matches!(
            node,
            Expression::Sub(
                lhs_handle,
                (),
                rhs
            )
            if arena[lhs_handle] == Expression::Number(1)
            && *rhs == Expression::Number(2)
        ));

        let (node, arena) = grammar::parse("1 - 2 - 3").unwrap();
        assert!(matches!(
            node,
            Expression::Sub(
                lhs_handle,
                (),
                rhs
            )
            if matches!(
                &arena[lhs_handle],
                Expression::Sub(
                    lhs_inner_handle,
                    (),
                    rhs_inner
                )
                if arena[*lhs_inner_handle] == Expression::Number(1)
                && **rhs_inner == Expression::Number(2)
            )
            && *rhs == Expression::Number(3)
        ));

        let (node, arena) = grammar::parse("1 - 2 * 3").unwrap();
        assert!(matches!(
            node,
            Expression::Sub(
                lhs_handle,
                (),
                rhs
            )
            if arena[lhs_handle] == Expression::Number(1)
            && *rhs == Expression::Mul(
                Box::new(Expression::Number(2)),
                (),
                Box::new(Expression::Number(3))
            )
        ));

        let (node, _arena) = grammar::parse("1 * 2 * 3").unwrap();
        assert_eq!(
            node,
            Expression::Mul(
                Box::new(Expression::Mul(
                    Box::new(Expression::Number(1)),
                    (),
                    Box::new(Expression::Number(2))
                )),
                (),
                Box::new(Expression::Number(3))
            )
        );

        let (node, arena) = grammar::parse("1 * 2 - 3").unwrap();
        assert!(matches!(
            node,
            Expression::Sub(
                lhs_handle,
                (),
                rhs
            )
            if arena[lhs_handle] == Expression::Mul(
                Box::new(Expression::Number(1)),
                (),
                Box::new(Expression::Number(2))
            )
            && *rhs == Expression::Number(3)
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
