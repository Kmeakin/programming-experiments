use std::hint::black_box;

use criterion::{BenchmarkId, Criterion, Throughput};

const INPUT: &str = r#"
fn foo() {
    // This is a comment
    let x = 42;
    let y = "Hello, world!";
    let z = 'a';
    let arr = [1, 2, 3];
    let tuple = (x, y, z);
    let _ = arr[0];
    if x > 10 {
        println!("x is greater than 10");
    } else {
        println!("x is less than or equal to 10");
    }
    (foo, bar, baz);
    if true {
        if false {
            if true {
                println!("This is a nested if statement");
            }
        } else {
            println!("This is the else branch of the nested if statement");
        }
    }
    // line comment
    // another line comment
    // very long line comment that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and
    for foo in bar {
        if false {
            if true {
                println!("This is a nested if statement");
            }
        } else {
            println!("This is the else branch of the nested if statement");
        }
    }
}
"#;

const _: [u8; 1024] = [0; INPUT.len()];

fn gen_input(repeat: usize) -> String { INPUT.repeat(repeat) }

fn logos(c: &mut Criterion) {
    let mut group = c.benchmark_group("logos::collect");
    for repeat in &[1, 2, 4, 8] {
        let input = gen_input(*repeat);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(repeat),
            input.as_str(),
            |b, input| {
                b.iter(|| {
                    let output = fast_lexer::logos::lex_iter(input).collect::<Vec<_>>();
                    black_box(output)
                });
            },
        );
    }
    group.finish();
    let mut group = c.benchmark_group("logos::collect_preallocated");
    for repeat in &[1, 2, 4, 8] {
        let input = gen_input(*repeat);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(repeat),
            input.as_str(),
            |b, input| {
                b.iter(|| {
                    let mut output = Vec::with_capacity(input.len());
                    output.extend(fast_lexer::logos::lex_iter(input));
                    black_box(output)
                });
            },
        );
    }
    group.finish();
}

fn manual(c: &mut Criterion) {
    let mut group = c.benchmark_group("manual::collect");
    for repeat in &[1, 2, 4, 8] {
        let input = gen_input(*repeat);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(repeat),
            input.as_str(),
            |b, input| {
                b.iter(|| {
                    let output = fast_lexer::manual::lex_iter(input).collect::<Vec<_>>();
                    black_box(output)
                });
            },
        );
    }
    group.finish();
    let mut group = c.benchmark_group("manual::collect_preallocated");
    for repeat in &[1, 2, 4, 8] {
        let input = gen_input(*repeat);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(repeat),
            input.as_str(),
            |b, input| {
                b.iter(|| {
                    let mut output = Vec::with_capacity(input.len());
                    output.extend(fast_lexer::manual::lex_iter(input));
                    black_box(output)
                });
            },
        );
    }
    group.finish();
}

fn main() {
    let mut criterion: Criterion<_> = Criterion::default().configure_from_args();
    logos(&mut criterion);
    manual(&mut criterion);
    Criterion::default().configure_from_args().final_summary();
}
