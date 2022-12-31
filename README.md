# Rust Sitter
Rust Sitter makes it easy to create efficient parsers in Rust by leveraging the [Tree Sitter](https://tree-sitter.github.io/tree-sitter/) parser generator. With Rust Sitter, you can define your entire grammar with annotations on idiomatic Rust code, and let macros generate the parser and type-safe bindings for you!

## Installation
First, add Rust/Tree Sitter to your `Cargo.toml`:
```toml
[dependencies]
rust-sitter = "0.1.2"

[build-dependencies]
rust-sitter-tool = "0.1.2"
```

The first step is to configure your `build.rs` to compile and link the generated Tree Sitter parser:

```rust
use std::path::PathBuf;

fn main() {
    rust_sitter_tool::build_parsers(&PathBuf::from("src/main.rs"));
}
```

## Defining a Grammar
Now that we have Rust Sitter added to our project, we can define our grammar. Rust Sitter grammars are defined in annotated Rust modules. First, we define the module that will contain our grammar

```rust
#[rust_sitter::grammar("arithmetic")]
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

Now that we have the type defined, we must annotate the enum variants to describe how to identify them in the text being parsed. First, we can apply `rust_sitter::leaf` to use a regular expression to match digits corresponding to a number, and define a transformation that parses the resulting string into a `u32`.

```rust
Number(
    #[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())]
    u32,
)
```

For the `Add` variant, things are a bit more complicated. First, we add an extra field corresponding to the `+` that must sit between the two sub-expressions. This can be achieved with `text` parameter of `rust_sitter::leaf`, which instructs the parser to match a specific string. Because we are parsing to `()`, we do not need to provide a transformation.

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
#[rust_sitter::grammar("arithmetic")]
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

## Special Types
Rust Sitter has a few special types that can be used to define more complex grammars.

### `Vec<T>`
To parse repeating structures, you can use a `Vec<T>` to parse a list of `T`s. Note that the `Vec<T>` type **cannot** be wrapped in another `Vec` (create additional structs if this is necessary). There are two special attributes that can be applied to a `Vec` field to control the parsing behavior.

The `#[rust_sitter::delimited(...)]` attribute can be used to specify a separator between elements of the list, and takes a parameter of the same format as an unnamed field. For example, we can define a grammar that parses a comma-separated list of expressions:

```rust
pub struct CommaSeparatedExprs {
    #[rust_sitter::delimited(
        #[rust_sitter::leaf(text = ",")]
        ()
    )]
    numbers: Vec<Expr>,
}
```

The `#[rust_sitter::repeat(...)]` attribute can be used to specify additional configuration for the parser. Currently, there is only one available parameter: `non_empty`, which takes a boolean that specifies if the list must contain at least one element. For example, we can define a grammar that parses a non-empty comma-separated list of numbers:

```rust
pub struct CommaSeparatedExprs {
    #[rust_sitter::repeat(non_empty = true)]
    #[rust_sitter::delimited(
        #[rust_sitter::leaf(text = ",")]
        ()
    )]
    numbers: Vec<Expr>,
}
```

### `Option<T>`
To parse optional structures, you can use an `Option<T>` to parse a single `T` or nothing. Like `Vec`, the `Option<T>` type **cannot** be wrapped in another `Option` (create additional structs if this is necessary). For example, we can make the list elements in the previous example optional so we can parse strings like `1,,2`:

```rust
pub struct CommaSeparatedExprs {
    #[rust_sitter::repeat(non_empty = true)]
    #[rust_sitter::delimited(
        #[rust_sitter::leaf(text = ",")]
        ()
    )]
    numbers: Vec<Option<Expr>>,
}
```

### `rust_sitter::Spanned<T>`
When using Rust Sitter to power diagnostic tools, it can be helpful to access spans marking the sections of text corresponding to a parsed node. To do this, you can use the `Spanned<T>` type, which captures the underlying parsed `T` and a pair of indices for the start (inclusive) and end (exclusive) of the corresponding substring. `Spanned` types can be used anywhere, and do not affect the parsing logic. For example, we could capture the spans of the expressions in our previous example:

```rust
pub struct CommaSeparatedExprs {
    #[rust_sitter::repeat(non_empty = true)]
    #[rust_sitter::delimited(
        #[rust_sitter::leaf(text = ",")]
        ()
    )]
    numbers: Vec<Option<Spanned<Expr>>>,
}
```

### `Box<T>`
Boxes are automatically constructed around the inner type when parsing, but Rust Sitter doesn't do anything extra beyond that.
