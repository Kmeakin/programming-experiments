//! Test lexers against the rustc implementation.

#![allow(clippy::needless_pass_by_value)]

use criterion::Criterion;
use criterion::measurement::{Measurement, WallTime};
use rust_lexer::common::{Lexer, Rustc};
use rust_lexer::lexers::scalar::Scalar;
use rust_lexer::lexers::scalar_loop_match::ScalarLoopMatch;
use rust_lexer::lexers::scalar_tail_call::ScalarTailCall;

const RUST_AMALGAMATION: &str = include_str!("../test-data/rust.rs");

fn bench<M: Measurement>(
    name: &str,
    measurement_name: &str,
    c: &mut Criterion<M>,
    lexer: impl Lexer,
) {
    let src = RUST_AMALGAMATION;

    let mut out = Vec::with_capacity(src.len());
    let mut out_kinds = Vec::with_capacity(src.len() + 16);
    let mut out_ends = Vec::with_capacity(src.len() + 16);

    c.benchmark_group(name)
        .throughput(criterion::Throughput::Bytes(src.len() as u64))
        .bench_function(format!("lex_str_to_vec::<{measurement_name}>"), |b| {
            b.iter(|| lexer.lex_str_to_vec(src, &mut out));
        })
        .bench_function(format!("lex_str_to_soa::<{measurement_name}>"), |b| {
            b.iter(|| lexer.lex_str_to_soa(src, &mut out_kinds, &mut out_ends));
        });
}

fn main() {
    #[cfg(target_os = "macos")]
    {
        use darwin_kperf_criterion::HardwareCounter;

        if false {
            let mut c = Criterion::default()
                .configure_from_args()
                .with_measurement(HardwareCounter::instructions().unwrap());
            bench("rustc", "instrs", &mut c, Rustc {});
            bench("scalar", "instrs", &mut c, Scalar {});
        }

        {
            let mut c = Criterion::default()
                .configure_from_args()
                .with_measurement(HardwareCounter::cycles().unwrap());
            bench("rustc", "cycles", &mut c, Rustc {});
            bench("scalar", "cycles", &mut c, Scalar {});
            bench("scalar_loop_match", "cycles", &mut c, ScalarLoopMatch {});
            bench("scalar_tail_call", "cycles", &mut c, ScalarTailCall {});
        }
    }

    #[cfg(target_os = "linux")]
    {
        use criterion_perf_events::Perf;
        use perfcnt::linux::{HardwareEventType as Hardware, PerfCounterBuilderLinux as Builder};

        {
            let mut c = Criterion::default()
                .configure_from_args()
                .with_measurement(Perf::new(Builder::from_hardware_event(
                    Hardware::Instructions,
                )));
            bench("rustc", "instrs", &mut c, Rustc {});
            bench("scalar", "instrs", &mut c, Scalar {});
        }

        {
            let mut c = Criterion::default()
                .configure_from_args()
                .with_measurement(Perf::new(Builder::from_hardware_event(Hardware::Cycles)));
            bench("rustc", "cycles", &mut c, Rustc {});
            bench("scalar", "cycles", &mut c, Scalar {});
        }
    }

    {
        let mut c = Criterion::default()
            .configure_from_args()
            .with_measurement(WallTime);
        bench("rustc", "wall_time", &mut c, Rustc {});
        bench("scalar", "wall_time", &mut c, Scalar {});
        bench("scalar_loop_match", "wall_time", &mut c, ScalarLoopMatch {});
        bench("scalar_tail_call", "wall_time", &mut c, ScalarTailCall {});
    }
}
