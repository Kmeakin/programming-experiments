use std::hint::black_box;
use std::ptr;

use criterion::{Criterion, Throughput};
use fast_rust_lexer::utils::push_unchecked;
use fast_rust_lexer::{multi_pass, multi_pass2, raw_ptr, simd, simd2, simd3};

const CRATE_ROOT: &str = env!("CARGO_MANIFEST_DIR");

fn get_input() -> String {
    std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap()
}

fn rustc(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("rustc");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("count", |b| {
        b.iter(|| {
            let mut count = 0;
            fast_rust_lexer::rustc::lex_iter(&input).for_each(|_| {
                count += 1;
            });
            black_box(count)
        });
    });

    group.bench_function("collect", |b| {
        b.iter(|| {
            let output = fast_rust_lexer::rustc::lex_iter(&input).collect::<Vec<_>>();
            black_box(output)
        });
    });

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            output.extend(fast_rust_lexer::rustc::lex_iter(&input));
            black_box(output)
        });
    });

    group.bench_function("push_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::rustc::lex_iter(&input).for_each(|(kind, len)| unsafe {
                push_unchecked(&mut output, (kind, len));
            });
            black_box(output)
        });
    });

    group.finish();
}

fn logos(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("logos");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("count", |b| {
        b.iter(|| {
            let mut count = 0;
            fast_rust_lexer::logos::lex_iter(&input).for_each(|_| {
                count += 1;
            });
            black_box(count)
        });
    });

    group.bench_function("collect", |b| {
        b.iter(|| {
            let output = fast_rust_lexer::logos::lex_iter(&input).collect::<Vec<_>>();
            black_box(output)
        });
    });

    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            output.extend(fast_rust_lexer::logos::lex_iter(&input));
            black_box(output)
        });
    });

    group.bench_function("push_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::logos::lex_iter(&input).for_each(|(kind, len)| unsafe {
                push_unchecked(&mut output, (kind, len));
            });
            black_box(output)
        });
    });

    group.finish();
}

fn manual(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("manual");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("count", |b| {
        b.iter(|| {
            let mut count = 0;
            fast_rust_lexer::manual::lex_iter(&input).for_each(|_| {
                count += 1;
            });
            black_box(count)
        });
    });

    group.bench_function("collect", |b| {
        b.iter(|| {
            let output = fast_rust_lexer::manual::lex_iter(&input).collect::<Vec<_>>();
            black_box(output)
        });
    });

    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            output.extend(fast_rust_lexer::manual::lex_iter(&input));
            black_box(output)
        });
    });

    group.bench_function("push_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::manual::lex_iter(&input).for_each(|(kind, len)| unsafe {
                push_unchecked(&mut output, (kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("push_very_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            let mut ptr: *mut (_, _) = output.as_mut_ptr();
            fast_rust_lexer::manual::lex_iter(&input).for_each(|(kind, len)| unsafe {
                ptr.write((kind, len));
                ptr = ptr.add(1);
            });
            unsafe {
                let len = ptr.offset_from_unsigned(output.as_mut_ptr());
                output.set_len(len);
            }
            black_box(output)
        });
    });

    group.finish();
}

fn manual_loop(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("manual_loop");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("count", |b| {
        b.iter(|| {
            let mut count = 0;
            fast_rust_lexer::manual_loop::lex_loop(&input, |_, _| {
                count += 1;
            });
            black_box(count)
        });
    });

    group.bench_function("collect", |b| {
        b.iter(|| {
            let mut output = Vec::new();
            fast_rust_lexer::manual_loop::lex_loop(&input, |kind, len| output.push((kind, len)));
            black_box(output)
        });
    });

    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::manual_loop::lex_loop(&input, |kind, len| output.push((kind, len)));
            black_box(output)
        });
    });

    group.bench_function("push_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::manual_loop::lex_loop(&input, |kind, len| unsafe {
                push_unchecked(&mut output, (kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("push_very_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            let mut ptr: *mut (_, _) = output.as_mut_ptr();
            fast_rust_lexer::manual_loop::lex_loop(&input, |kind, len| unsafe {
                ptr.write((kind, len));
                ptr = ptr.add(1);
            });
            unsafe {
                let len = ptr.offset_from_unsigned(output.as_mut_ptr());
                output.set_len(len);
            }
            black_box(output)
        });
    });
    group.finish();
}

fn jump_threading(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("jump_threading");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("count", |b| {
        b.iter(|| {
            let mut count = 0;
            fast_rust_lexer::jump_threading::lex_loop(input.as_bytes(), |_, _| {
                count += 1;
            });
            black_box(count)
        });
    });

    group.bench_function("collect", |b| {
        b.iter(|| {
            let mut output = Vec::new();
            fast_rust_lexer::jump_threading::lex_loop(input.as_bytes(), |kind, len| {
                output.push((kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::jump_threading::lex_loop(input.as_bytes(), |kind, len| {
                output.push((kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("push_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            fast_rust_lexer::jump_threading::lex_loop(input.as_bytes(), |kind, len| unsafe {
                push_unchecked(&mut output, (kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("push_very_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            let mut ptr: *mut (_, _) = output.as_mut_ptr();
            fast_rust_lexer::jump_threading::lex_loop(input.as_bytes(), move |kind, len| unsafe {
                ptr.write((kind, len));
                ptr = ptr.add(1);
            });
            unsafe {
                let len = ptr.offset_from_unsigned(output.as_mut_ptr());
                output.set_len(len);
            }
            black_box(output)
        });
    });
    group.finish();
}

fn raw_ptr<const VEC_LEN: usize>(c: &mut Criterion) {
    let input = get_input();
    let mut input = input.into_bytes();
    input.extend([raw_ptr::EOF_BYTE; VEC_LEN]);
    let input = input.as_slice();

    let mut group = c.benchmark_group(format!("raw_ptr::<{VEC_LEN}>"));
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("count", |b| {
        b.iter(|| {
            let mut count = 0;
            raw_ptr::lex_loop::<VEC_LEN, _>(input, |_, _, _| {
                count += 1;
            });
            black_box(count)
        });
    });

    group.bench_function("collect", |b| {
        b.iter(|| {
            let mut output = Vec::new();
            raw_ptr::lex_loop::<VEC_LEN, _>(input, |kind, start, end| {
                let len = unsafe { end.offset_from_unsigned(start) };
                output.push((kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("collect_preallocated", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            raw_ptr::lex_loop::<VEC_LEN, _>(input, |kind, start, end| {
                let len = unsafe { end.offset_from_unsigned(start) };
                output.push((kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("push_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            raw_ptr::lex_loop::<VEC_LEN, _>(input, |kind, start, end| unsafe {
                let len = end.offset_from_unsigned(start);
                push_unchecked(&mut output, (kind, len));
            });
            black_box(output)
        });
    });

    group.bench_function("push_very_unchecked", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            let mut ptr: *mut (_, _) = output.as_mut_ptr();
            raw_ptr::lex_loop::<VEC_LEN, _>(input, move |kind, start, end| unsafe {
                let len = end.offset_from_unsigned(start);
                ptr.write((kind, len));
                ptr = ptr.add(1);
            });
            unsafe {
                let len = ptr.offset_from_unsigned(output.as_mut_ptr());
                output.set_len(len);
            }
            black_box(output)
        });
    });
    group.finish();
}

fn check_unicode(c: &mut Criterion) {
    let input = get_input();

    let mut group = c.benchmark_group("check_unicode");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("from_utf8_std", |b| {
        b.iter(|| {
            let output = str::from_utf8(input.as_bytes()).is_ok();
            black_box(output)
        });
    });

    group.finish();
}

fn memcpy(c: &mut Criterion) {
    let mut input = get_input();

    let mut group = c.benchmark_group("memcpy");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("to_ascii_uppercase", |b| {
        b.iter(|| {
            input.make_ascii_lowercase();
        });
    });

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("string_clone", |b| {
        let mut output = input.clone();
        b.iter(|| {
            unsafe { ptr::copy_nonoverlapping(input.as_ptr(), output.as_mut_ptr(), input.len()) };
        });
    });

    group.finish();
}

fn multi_pass<W: multi_pass::Word, const VEC_LEN: usize>(c: &mut Criterion) {
    let input = get_input();
    let (input, mut bitmasks) = multi_pass::prepare_input::<W, VEC_LEN>(&input);

    let mut group = c.benchmark_group("multi_pass");

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function(format!("multi_pass/stage1::<{VEC_LEN}>"), |b| {
        b.iter(|| multi_pass::stage1::<W, VEC_LEN>(&input, &mut bitmasks));
    });

    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function(format!("multi_pass/stage2::<{VEC_LEN}>"), |b| {
        let mut output = vec![0xff; input.len() * 5];
        b.iter(|| {
            let out = multi_pass::stage2::<W, VEC_LEN>(
                &input,
                bitmasks.each_ref().map(Vec::as_slice),
                &mut output,
            );
            black_box(out);
        });
    });

    group.finish();
}

fn simd(c: &mut Criterion) {
    let input = get_input();
    let mut input = input.into_bytes();
    input.extend([simd::EOF_BYTE; 128]);
    let input = input.as_slice();

    let mut group = c.benchmark_group("simd");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("simd::lex::<16>", |b| {
        let mut output = vec![0xff; input.len() * 5];
        b.iter(|| {
            let output = simd::lex::<16>(input, &mut output);
            black_box(output);
        });
    });

    group.bench_function("simd::lex::<32>", |b| {
        let mut output = vec![0xff; input.len() * 5];
        b.iter(|| {
            let output = simd::lex::<32>(input, &mut output);
            black_box(output);
        });
    });

    group.bench_function("simd::lex::<64>", |b| {
        let mut output = vec![0xff; input.len() * 5];
        b.iter(|| {
            let output = simd::lex::<64>(input, &mut output);
            black_box(output);
        });
    });

    group.finish();
}

fn simd2<const VEC_LEN: usize>(c: &mut Criterion) {
    let input = get_input();
    let input = simd2::prepare_input::<VEC_LEN>(&input);

    let mut group = c.benchmark_group("simd2");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function(format!("simd2::lex::<{VEC_LEN}>"), |b| {
        let mut output = vec![0xff; input.len() * 5];
        b.iter(|| {
            let output = simd2::lex::<VEC_LEN>(&input, &mut output);
            black_box(output);
        });
    });

    group.finish();
}

fn simd3<const VEC_LEN: usize>(c: &mut Criterion) {
    let input = get_input();
    let input = simd2::prepare_input::<VEC_LEN>(&input);

    let mut group = c.benchmark_group("simd3");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function(format!("simd3::lex::<{VEC_LEN}>"), |b| {
        let mut output = vec![0xff; input.len() * 5];
        b.iter(|| {
            let output = simd3::lex::<VEC_LEN>(&input, &mut output);
            black_box(output);
        });
    });

    group.finish();
}

fn multi_pass2<const VEC_LEN: usize>(c: &mut Criterion) {
    let input = get_input();
    let (input, mut output) = multi_pass2::prepare_input::<VEC_LEN>(&input);

    let mut group = c.benchmark_group("multi_pass2");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function(format!("multi_pass2::stage1::<{VEC_LEN}>"), |b| {
        b.iter(|| {
            let output = multi_pass2::stage1::<VEC_LEN>(&input, &mut output);
            black_box(output);
        });
    });

    let indices = multi_pass2::stage1::<VEC_LEN>(&input, &mut output);
    group.bench_function(format!("multi_pass2::stage2::<{VEC_LEN}>"), |b| {
        b.iter(|| {
            let output = multi_pass2::stage2::<VEC_LEN>(&input, indices);
            black_box(output);
        });
    });

    group.finish();
}

fn main() {
    let mut criterion: Criterion<_> = Criterion::default().configure_from_args();
    check_unicode(&mut criterion);
    memcpy(&mut criterion);
    rustc(&mut criterion);
    logos(&mut criterion);
    manual(&mut criterion);
    manual_loop(&mut criterion);
    jump_threading(&mut criterion);
    raw_ptr::<16>(&mut criterion);
    raw_ptr::<32>(&mut criterion);
    raw_ptr::<64>(&mut criterion);

    multi_pass::<u16, 16>(&mut criterion);
    multi_pass::<u32, 32>(&mut criterion);
    multi_pass::<u64, 64>(&mut criterion);

    simd2::<16>(&mut criterion);
    simd2::<32>(&mut criterion);
    simd2::<64>(&mut criterion);

    simd3::<16>(&mut criterion);
    simd3::<32>(&mut criterion);
    simd3::<64>(&mut criterion);

    multi_pass2::<16>(&mut criterion);
    multi_pass2::<32>(&mut criterion);
    multi_pass2::<64>(&mut criterion);

    Criterion::default().configure_from_args().final_summary();
}
