# Rust Sitter
[![Crates.io](https://img.shields.io/crates/v/rust-sitter)](https://crates.io/crates/rust-sitter)

Rust Sitter makes it easy to create efficient parsers in Rust by leveraging the [Tree Sitter](https://tree-sitter.github.io/tree-sitter/) parser generator. With Rust Sitter, you can define your entire grammar with annotations on idiomatic Rust code, and let macros generate the parser and type-safe bindings for you!

## Installation
First, add Rust/Tree Sitter to your `Cargo.toml`:
```toml
[dependencies]
rust-sitter = "0.4.4"

[build-dependencies]
rust-sitter-tool = "0.4.4"
```

_Note: By default, Rust Sitter uses a fork of Tree Sitter with a pure-Rust runtime to support `wasm32-unknown-unknown`. To use the standard C runtime instead, disable default features and enable the `tree-sitter-standard` feature_

The first step is to configure your `build.rs` to compile and link the generated Tree Sitter parser:

```rust
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src");
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
    Box<Expr>,
    #[rust_sitter::leaf(text = "+")] (),
    Box<Expr>,
)
```

If we try to compile this grammar, however, we will see ane error due to conflicting parse trees for expressions like `1 + 2 + 3`, which could be parsed as `(1 + 2) + 3` or `1 + (2 + 3)`. We want the former, so we can add a further annotation specifying that we want left-associativity for this rule.

```rust
#[rust_sitter::prec_left(1)]
Add(
    Box<Expr>,
    #[rust_sitter::leaf(text = "+")] (),
    Box<Expr>,
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
            Box<Expr>,
            #[rust_sitter::leaf(text = "+")] (),
            Box<Expr>,
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

## Type Annotations
Rust Sitter supports a number of annotations that can be applied to type and fields in your grammar. These annotations can be used to control how the parser behaves, and how the resulting AST is constructed.

### `#[rust_sitter::language]`
This annotation marks the entrypoint for parsing, and determines which AST type will be returned from parsing. Only one type in the grammar can be marked as the entrypoint.

```rust
#[rust_sitter::language]
struct Code {
    ...
}
````

### `#[rust_sitter::extra]`
This annotation marks a node as extra and can safely be skipped while parsing. This is useful for handling whitespace/newlines/comments.

```rust
#[rust_sitter::extra]
struct Whitespace {
    #[rust_sitter::leaf(pattern = r"\s")]
    _whitespace: (),
}
```

## Field Annotations
### `#[rust_sitter::leaf(...)]`
The `#[rust_sitter::leaf(...)]` annotation can be used to define a leaf node in the AST. This annotation takes a number of parameters that control how the parser behaves:
- the `pattern` parameter takes a regular expression that is used to match the text of the leaf node. This parameter is required.
- the `text` parameter takes a string that is used to match the text of the leaf node. This parameter is mutually exclusive with `pattern`.
- the `transform` parameter takes a function that is used to transform the matched text (an `&str`) into the desired type. This parameter is optional if the target type is `()`.

`leaf` can either be applied to a field in a struct / enum variant (as seen above), or directly on a type with no fields:

```rust
#[rust_sitter::leaf(text = "9")]
struct BigDigit;

enum SmallDigit {
    #[rust_sitter::leaf(text = "0")]
    Zero,
    #[rust_sitter::leaf(text = "1")]
    One,
}
```

### `#[rust_sitter::prec(...)]` / `#[rust_sitter::prec_left(...)]` / `#[rust_sitter::prec_right(...)]`
This annotation can be used to define a non/left/right-associative operator. This annotation takes a single parameter, which is the precedence level of the operator (higher binds more tightly).

### `#[rust_sitter::skip(...)]`
This annotation can be used to define a field that does not correspond to anything in the input string, such as some metadata. This annotation takes a single parameter, which is the value that should be used to populate that field at runtime.

### `#[rust_sitter::word]`
This annotation marks the field as a Tree Sitter [word](https://tree-sitter.github.io/tree-sitter/creating-parsers#keywords), which is useful when handling errors involving keywords. Only one field in the grammar can be marked as a word.

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


### `rust_sitter::Handle<T>`
Wherever you might use a `Box<T>`, you can instead use a `Handle<T>` to switch to an Arena pattern. If you use one or more `Handle`s in your grammar declaration, you need to also declare an Arena type for the objects to be stored in. This is done by adding a `#[rust_sitter::arena]` attribute to an empty struct declaration. For example:

```rust
#[rust_sitter::grammar("arithmetic")]
pub mod grammar {
    use rust_sitter::Handle;

    #[rust_sitter::arena]
    #[derive(Default, Debug)]
    pub struct MyArena;

    #[rust_sitter::language]
    #[derive(PartialEq, Eq, Debug)]
    pub enum Expression {
        Number(#[rust_sitter::leaf(pattern = r"\d+", transform = |v| v.parse().unwrap())] i32),
        #[rust_sitter::prec_left(1)]
        Add(
            Handle<Expression>,
            #[rust_sitter::leaf(text = "+")] (),
            Handle<Expression>,
        ),
    }
}
```

The `grammar::parse` method will then also return an instance of `MyArena`, which can be indexed into by the `Handle<Expression>` values.