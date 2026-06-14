//! Test lexers against the rustc implementation.

#![allow(clippy::needless_pass_by_value)]

use criterion::measurement::Measurement;
use criterion::{Bencher, BenchmarkId, Criterion, Throughput};
use rust_lexer::common::{Lexer, Rustc, SIMD_PADDING};
use rust_lexer::lexers::rustc::FrontmatterAllowed;
use rust_lexer::lexers::scalar::Scalar;
use rust_lexer::lexers::scalar_loop_match::ScalarLoopMatch;
use rust_lexer::lexers::scalar_tail_call::ScalarTailCall;

const PACKAGE_ROOT: &str = env!("CARGO_MANIFEST_DIR");

fn bench_soa<M: Measurement>(b: &mut Bencher<M>, lexer: impl Lexer, src: &str) {
    let mut out_kinds = Vec::with_capacity(src.len() + SIMD_PADDING);
    let mut out_ends = Vec::with_capacity(src.len() + SIMD_PADDING);
    b.iter(|| lexer.lex_str_to_soa(src, &mut out_kinds, &mut out_ends));
}

fn main() {
    let mut c = Criterion::default().configure_from_args();
    let mut group = c.benchmark_group("lexer");

    // Sort files by size to get deterministic order.
    let mut files = std::fs::read_dir(format!("{PACKAGE_ROOT}/test-data"))
        .unwrap()
        .map(|entry| entry.unwrap())
        .filter(|file| file.file_type().unwrap().is_file())
        .filter(|file| file.path().extension().is_some_and(|ext| ext == "rs"))
        .map(|file| {
            (
                file.file_name(),
                std::fs::read_to_string(file.path()).unwrap(),
            )
        })
        .collect::<Vec<_>>();
    files.sort_by_key(|(_, contents)| contents.len());

    for (name, src) in files {
        group.throughput(Throughput::Bytes(src.len() as u64));
        let name = name.to_string_lossy();

        group.bench_function(BenchmarkId::new("rustc", &name), |b| {
            bench_soa(b, Rustc {}, &src);
        });
        group.bench_function(BenchmarkId::new("scalar", &name), |b| {
            bench_soa(b, Scalar {}, &src);
        });
        group.bench_function(BenchmarkId::new("scalar_loop_match", &name), |b| {
            bench_soa(b, ScalarLoopMatch {}, &src);
        });
        group.bench_function(BenchmarkId::new("scalar_tail_call", &name), |b| {
            bench_soa(b, ScalarTailCall {}, &src);
        });
    }
    drop(group);
    drop(c);
}
