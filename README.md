# Rust Sitter
Rust Sitter makes it easy to create efficient parsers in Rust by leveraging the [Tree Sitter](https://tree-sitter.github.io/tree-sitter/) parser generator. With Rust Sitter, you can define your entire grammar with annotations on idiomatic Rust code, and let macros generate the parser and type-safe bindings for you!

## Quickstart
First, add Rust/Tree Sitter to your `Cargo.toml`:
```toml
[dependencies]
rust-sitter = "0.1.0"

[build-dependencies]
rust-sitter-tool = "0.1.0"
```

The first step is to configure your `build.rs` to compile and link the generated Tree Sitter parser:

```rust
use std::path::PathBuf;

fn main() {
    rust_sitter_tool::build_parsers(&PathBuf::from("src/main.rs"));
}
```

Now that we have Rust Sitter added to our project, we can define our grammar. Rust Sitter grammars are defined in annotated Rust modules. First, we define the module that will contain our grammar

```rust
#[rust_sitter::grammar]
mod grammar {

}
```

Then, inside the module, we can define individual AST nodes. For this simple example, we'll define an expression that can be used in a mathematical expression. Note that we annotate this type as `#[rust_sitter::language]` to indicate that it is the root AST type.

```rust
#[rust_sitter::language]
pub enum Expr {
    Number(u32),
    Add(Box<Expr>, Box<Expr>)
}
```

Now that we have the type defined, we must annotate the enum variants to describe how to identify them in the text being parsed. First, we can use a regular expression to match digits corresponding to a number, and define a transformation that parses the resulting string into a `u32`.

```rust
Number(
    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
    u32,
)
```

For the `Add` variant, things are a bit more complicated. First, we add an extra field corresponding to the `+` that must sit between the two sub-expressions.

```rust
Add(
    Box<Expression>,
    #[rust_sitter::leaf(text = "+")] (),
    Box<Expression>,
)
```

If we try to compile this grammar, however, we will see ane error due to conflicting parse trees for expressions like `1 + 2 + 3`, which could be parsed as `(1 + 2) + 3` or `1 + (2 + 3)`. We want the former, so we can add a further annotation specifying that we want left-associativity for this rule.

```rust
#[rust_sitter::prec_left(1)]
Add(
    Box<Expression>,
    #[rust_sitter::leaf(text = "+")] (),
    Box<Expression>,
)
```

All together, our grammar looks like this:

```rust
#[rust_sitter::grammar]
mod grammar {
    #[rust_sitter::language]
    pub enum Expr {
        Number(
            #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
            u32,
        ),
        #[rust_sitter::prec_left(1)]
        Add(
            Box<Expression>,
            #[rust_sitter::leaf(text = "+")] (),
            Box<Expression>,
        )
    }
}
```

We can then parse text using this grammar:

```rust
dbg!(grammar::parse("1+2+3"));
/*
grammar::parse("1+2+3") = Ok(Add(
    Add(
        Number(
            1,
        ),
        (),
        Number(
            2,
        ),
    ),
    (),
    Number(
        3,
    ),
))
*/
```
