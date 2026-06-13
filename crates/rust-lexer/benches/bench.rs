//! Test lexers against the rustc implementation.

#![allow(clippy::needless_pass_by_value)]

use criterion::measurement::Measurement;
use criterion::{Bencher, BenchmarkId, Criterion, Throughput};
use rust_lexer::common::{Lexer, Rustc};
use rust_lexer::lexers::scalar::Scalar;
use rust_lexer::lexers::scalar_loop_match::ScalarLoopMatch;
use rust_lexer::lexers::scalar_tail_call::ScalarTailCall;

const PACKAGE_ROOT: &str = env!("CARGO_MANIFEST_DIR");

fn bench_soa<M: Measurement>(b: &mut Bencher<M>, lexer: impl Lexer, src: &str) {
    let mut out_kinds = Vec::with_capacity(src.len() + 16);
    let mut out_ends = Vec::with_capacity(src.len() + 16);
    b.iter(|| lexer.lex_str_to_soa(src, &mut out_kinds, &mut out_ends));
}

fn main() {
    let mut c = Criterion::default().configure_from_args();
    let mut group = c.benchmark_group("lexer");
    for entry in std::fs::read_dir(format!("{PACKAGE_ROOT}/test-data")).unwrap() {
        let file = entry.unwrap();
        if file.file_type().unwrap().is_dir() {
            continue;
        }
        if file.path().extension().is_none_or(|ext| ext != "rs") {
            continue;
        }
        let file_name = file.file_name();
        let name = file_name.to_string_lossy();
        if name == "rust.rs" {
            continue;
        }
        let src = std::fs::read_to_string(file.path()).unwrap();
        group.throughput(Throughput::Bytes(src.len() as u64));

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
