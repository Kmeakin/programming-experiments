use std::hint::black_box;

use criterion::{Criterion, Throughput};

const CRATE_ROOT: &str = env!("CARGO_MANIFEST_DIR");

fn get_input() -> String {
    std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap()
}

fn rustc(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("rustc");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect", |b| {
        b.iter(|| {
            let output = fast_rust_lexer::rustc_lex_iter(&input).collect::<Vec<_>>();
            black_box(output)
        });
    });

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            output.extend(fast_rust_lexer::rustc_lex_iter(&input));
            black_box(output)
        });
    });
    group.finish();
}

fn logos(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("logos");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect", |b| {
        b.iter(|| {
            let output = fast_rust_lexer::logos::lex_iter(&input).collect::<Vec<_>>();
            black_box(output)
        });
    });

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            output.extend(fast_rust_lexer::logos::lex_iter(&input));
            black_box(output)
        });
    });
    group.finish();
}

fn manual(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("manual");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect", |b| {
        b.iter(|| {
            let output = fast_rust_lexer::manual::lex_iter(&input).collect::<Vec<_>>();
            black_box(output)
        });
    });

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            output.extend(fast_rust_lexer::manual::lex_iter(&input));
            black_box(output)
        });
    });
    group.finish();
}

fn main() {
    let mut criterion: Criterion<_> = Criterion::default().configure_from_args();
    rustc(&mut criterion);
    logos(&mut criterion);
    manual(&mut criterion);
    Criterion::default().configure_from_args().final_summary();
}
