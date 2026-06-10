//! Test lexers against the rustc implementation.

#![allow(clippy::needless_pass_by_value)]

use std::time::Duration;

use criterion::Criterion;
use rust_lexer::common::{Lexer, Rustc};
use rust_lexer::lexers::scalar::Scalar;

const RUST_AMALGAMATION: &str = include_str!("../test-data/rust.rs");

fn bench(name: &str, c: &mut Criterion, lexer: impl Lexer) {
    let src = RUST_AMALGAMATION;

    c.benchmark_group(name)
        .throughput(criterion::Throughput::Bytes(src.len() as u64))
        .bench_function("lex_str_to_vec", |b| {
            b.iter_custom(|iters| {
                let mut duration = Duration::ZERO;
                for _ in 0..iters {
                    duration += lexer.lex_str_to_vec(src);
                }
                duration
            });
        });
}

fn main() {
    let mut c = Criterion::default().configure_from_args();
    bench("rustc", &mut c, Rustc {});
    bench("scalar", &mut c, Scalar {});
}
