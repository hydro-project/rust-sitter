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
            if rng.next_u32() % 2 == 0 {
                write!(&mut s, " - ",).unwrap();
            } else {
                write!(&mut s, " * ",).unwrap();
            }
        }
        write!(&mut s, "{}", rng.next_u32() % (i32::MAX as u32)).unwrap();
    }
    s
}

fn boxes(src: &str) -> rust_sitter_example::arithmetic::grammar::Expression {
    rust_sitter_example::arithmetic::grammar::parse(src).unwrap()
}

fn handles(
    src: &str,
) -> (
    rust_sitter_example::arithmetic::grammar_handles::Expression,
    rust_sitter_example::arithmetic::grammar_handles::Arena,
) {
    rust_sitter_example::arithmetic::grammar_handles::parse(src).unwrap()
}

fn criterion_benchmark(c: &mut Criterion) {
    const SEED: u32 = 0;
    const LEN: usize = 10000;
    let src = gen_big_arithmetic_expr(SEED, LEN);
    c.bench_function("boxes", |b| b.iter(|| boxes(black_box(&src))));
    c.bench_function("handles", |b| b.iter(|| handles(black_box(&src))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
