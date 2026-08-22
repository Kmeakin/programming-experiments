use std::fmt;
use std::str::FromStr;

#[derive(Copy, Clone, Debug)]
enum LexerKind {
    Rustc,
    Scalar,
    LoopMatch,
    TailCall,
}

impl LexerKind {
    const ALL: &'static [Self] = &[Self::Rustc, Self::Scalar, Self::LoopMatch, Self::TailCall];
}

impl fmt::Display for LexerKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Rustc => f.write_str("rustc"),
            Self::Scalar => f.write_str("scalar"),
            Self::LoopMatch => f.write_str("loop-match"),
            Self::TailCall => f.write_str("tail-call"),
        }
    }
}

fn parse_lexers(args: &mut impl Iterator<Item = String>) -> Result<Vec<LexerKind>, String> {
    let mut lexers = Vec::new();
    for arg in args.by_ref() {
        match arg.as_str() {
            "rustc" => lexers.push(LexerKind::Rustc),
            "scalar" => lexers.push(LexerKind::Scalar),
            "loop-match" => lexers.push(LexerKind::LoopMatch),
            "tail-call" => lexers.push(LexerKind::TailCall),
            _ => {
                return Err(format!(
                    "Unknown lexer '{arg}'. Valid options are: rustc, scalar, loop-match, \
                     tail-call"
                ));
            }
        }
    }
    Ok(lexers)
}

fn parse_int(args: &mut impl Iterator<Item = String>) -> Result<u32, String> {
    let Some(arg) = args.next() else { todo!() };
    match u32::from_str(&arg) {
        Ok(x) => Ok(x),
        Err(err) => Err(format!(
            "Failed to parse integer from argument '{arg}': {err}"
        )),
    }
}

fn main() -> Result<(), String> {
    let args = std::env::args().skip(1);
    let mut files = Vec::new();
    let mut lexers = LexerKind::ALL.to_vec();
    let mut iters = 10;
    let mut iter = args;
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-h" | "--help" => {
                usage();
                return Ok(());
            }
            "--iters" => iters = parse_int(&mut iter)?,
            "--lexers" => lexers = parse_lexers(&mut iter)?,
            _ => files.push(arg),
        }
    }

    files.sort();

    for file in files {
        let src = match std::fs::read_to_string(&file) {
            Ok(src) => src,
            Err(err) => {
                eprintln!("Warning: Failed to read file '{file}': {err}");
                continue;
            }
        };
        for lexer in &lexers {
            let mut best_duration = std::time::Duration::MAX;
            for _ in 0..iters {
                use rust_lexer::common::{Lexer as _, Rustc};
                use rust_lexer::lexers::scalar::Scalar;
                use rust_lexer::lexers::scalar_loop_match::ScalarLoopMatch;
                use rust_lexer::lexers::scalar_tail_call::ScalarTailCall;

                let mut tokens = Vec::with_capacity(src.len());
                let mut ends = Vec::with_capacity(src.len());
                let start = std::time::Instant::now();
                match lexer {
                    LexerKind::Rustc => Rustc {}.lex_str_to_soa(&src, &mut tokens, &mut ends),
                    LexerKind::Scalar => Scalar {}.lex_str_to_soa(&src, &mut tokens, &mut ends),
                    LexerKind::LoopMatch => {
                        ScalarLoopMatch {}.lex_str_to_soa(&src, &mut tokens, &mut ends);
                    }
                    LexerKind::TailCall => {
                        ScalarTailCall {}.lex_str_to_soa(&src, &mut tokens, &mut ends);
                    }
                }
                let duration = start.elapsed();
                best_duration = best_duration.min(duration);
            }
            let throughput_bytes = (src.len() as f64) / best_duration.as_secs_f64();
            println!(
                "{file:50} {lexer: <20} {throughput:5.2} MiB/s",
                lexer = lexer.to_string(),
                throughput = throughput_bytes / (1024.0 * 1024.0)
            );
        }
        println!();
    }

    Ok(())
}

fn usage() {
    eprintln!("Usage: rust-lexer FILE [FILE ...]");
}
