use criterion::{criterion_group, criterion_main, Criterion};
use rand::{RngCore, SeedableRng};
use std::{fmt::Write, hint::black_box};

fn gen_big_arithmetic_expr(seed: u32, len: usize) -> String {
    let mut big_seed = [0u8; 32];
    big_seed[0..4].copy_from_slice(&u32::to_le_bytes(seed));
    let mut rng = rand::rngs::SmallRng::from_seed(big_seed);

    let mut s = String::new();
    for i in 0..len {
        if i != 0 {
            match rng.next_u32() % 16 {
                0 => write!(&mut s, " - "),
                1..3 => write!(&mut s, " + "),
                3..7 => write!(&mut s, " * "),
                _ => write!(&mut s, " * "),
            }
            .unwrap()
        }
        write!(&mut s, "{}", rng.next_u32() % (i32::MAX as u32)).unwrap();
    }
    s
}

/// Just parse the expression - don't generate AST.
fn parse(src: &str) -> rust_sitter::tree_sitter::Tree {
    let mut parser = rust_sitter::tree_sitter::Parser::new();
    parser
        .set_language(&rust_sitter_benchmarking::box_grammar::language())
        .unwrap();
    parser.parse(src, None).unwrap()
}

/// Traverse and place AST into boxes.
fn boxes(src: &str) -> rust_sitter_benchmarking::box_grammar::Expression {
    rust_sitter_benchmarking::box_grammar::parse(src).unwrap()
}

/// Traverse and place AST into handles in an arena.
fn handles(
    src: &str,
) -> (
    rust_sitter_benchmarking::handle_grammar::Expression,
    rust_sitter_benchmarking::handle_grammar::Arena,
) {
    rust_sitter_benchmarking::handle_grammar::parse(src).unwrap()
}

fn criterion_benchmark(c: &mut Criterion) {
    const SEED: u32 = 0;
    const LEN: usize = 1000;
    let src = gen_big_arithmetic_expr(SEED, LEN);
    c.bench_function("parse", |b| b.iter(|| parse(black_box(&src))));
    c.bench_function("boxes", |b| b.iter(|| boxes(black_box(&src))));
    c.bench_function("handles", |b| b.iter(|| handles(black_box(&src))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
