use criterion::{Criterion, Throughput};
use rust_lexer::TokenKind;
use rust_lexer::lexers::rustc::FrontmatterAllowed;
use rust_lexer::lexers::{self};

const TEST_DATA_DIR: &str = "test-data";
const WORKSPACE_ROOT: &str = env!("CARGO_MANIFEST_DIR");

fn main() {
    let mut c = Criterion::default().configure_from_args();

    for file in std::fs::read_dir(format!("{WORKSPACE_ROOT}/{TEST_DATA_DIR}")).unwrap() {
        let file = file.unwrap();
        let path = file.path();
        if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            let src = std::fs::read_to_string(&path).unwrap();
            let mut g = c.benchmark_group(path.display().to_string());
            g.throughput(Throughput::Bytes(src.len() as u64));

            g.bench_function("rustc", |b| {
                b.iter_custom(|iters| {
                    let mut tokens = Vec::with_capacity(src.len());
                    let start = std::time::Instant::now();
                    for _ in 0..iters {
                        tokens.clear();
                        for token in lexers::rustc::tokenize(&src, FrontmatterAllowed::No) {
                            tokens.push(token);
                        }
                    }
                    let time = start.elapsed();
                    drop(tokens);
                    time
                });
            });

            g.bench_function("rustc_flattened", |b| {
                b.iter_custom(|iters| {
                    let mut tokens = Vec::with_capacity(src.len());
                    let start = std::time::Instant::now();
                    for _ in 0..iters {
                        tokens.clear();
                        for token in lexers::rustc::tokenize(&src, FrontmatterAllowed::No) {
                            tokens.push((TokenKind::from(token.kind), token.len));
                        }
                    }
                    let time = start.elapsed();
                    drop(tokens);
                    time
                });
            });

            g.finish();
        }
    }

    c.final_summary();
}
