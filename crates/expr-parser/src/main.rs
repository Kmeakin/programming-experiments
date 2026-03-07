use std::ops::ControlFlow;

use expr_parser::lexer::{ErrorKind, TokenKind, TokenSink};

struct TokenCounter {
    tokens: usize,
    errors: usize,
}

impl TokenSink for TokenCounter {
    type Break = std::convert::Infallible;
    fn token(&mut self, _kind: TokenKind, _text: &str) -> ControlFlow<Self::Break> {
        self.tokens += 1;
        ControlFlow::Continue(())
    }
    fn error(&mut self, _kind: ErrorKind, _text: &str) -> ControlFlow<Self::Break> {
        self.errors += 1;
        ControlFlow::Continue(())
    }
}

struct Printer {}
impl TokenSink for Printer {
    type Break = std::convert::Infallible;
    fn token(&mut self, kind: TokenKind, text: &str) -> ControlFlow<Self::Break> {
        println!("Token: {kind:?}\t{text:?}");
        ControlFlow::Continue(())
    }
    fn error(&mut self, kind: ErrorKind, text: &str) -> ControlFlow<Self::Break> {
        println!("Error: {kind:?}\t{text:?}");
        ControlFlow::Continue(())
    }
}

fn main() {
    let args = std::env::args();
    let args: Vec<_> = args.collect();

    if args.is_empty() {
        eprintln!("expr-parser: [--timings] FILES");
        return;
    }

    match args.as_slice() {
        [] | [_] => {
            eprintln!("expr-parser: [--timings] FILES");
        }
        [_, arg1, files @ ..] if arg1 == "--timings" => {
            for file in files {
                let text = match std::fs::read_to_string(file) {
                    Err(err) => {
                        eprintln!("Cannot read `{file}`: {err}");
                        continue;
                    }
                    Ok(text) => text,
                };
                let mut sink = TokenCounter {
                    tokens: 0,
                    errors: 0,
                };
                let start_time = std::time::Instant::now();
                expr_parser::lexer::lex(&text, &mut sink);
                let TokenCounter { tokens, errors } = sink;
                let end_time = std::time::Instant::now();
                let elapsed = end_time - start_time;
                let bytes = text.len();
                let lines = text.lines().count();
                let bytes_per_sec = bytes as f64 / elapsed.as_secs_f64() / 1E6;
                let lines_per_sec = lines as f64 / elapsed.as_secs_f64() / 1E6;
                println!(
                    "\
{file}:
\t{elapsed:?}
\t{bytes} bytes
\t{lines} lines
\t{tokens} tokens
\t{errors} errors
\t{bytes_per_sec} MB/sec
\t{lines_per_sec} Mlines/sec"
                );
            }
        }
        [_, files @ ..] => {
            for file in files {
                let text = match std::fs::read_to_string(file) {
                    Err(err) => {
                        eprintln!("Cannot read `{file}`: {err}");
                        continue;
                    }
                    Ok(text) => text,
                };

                let sink = Printer {};
                expr_parser::lexer::lex(&text, sink);
            }
        }
    }
}
