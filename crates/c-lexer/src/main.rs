fn main() {
    let args = std::env::args();
    let args: Vec<_> = args.collect();

    if args.is_empty() {
        eprintln!("c-lexer: [--timings] FILES");
        return;
    }

    match args.as_slice() {
        [] | [_] => {
            eprintln!("c-lexer: [--timings] FILES");
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
                let start_time = std::time::Instant::now();
                let tokens = c_lexer::lex(&text);
                let count = tokens.count();
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
\t{count} tokens
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
                let tokens = c_lexer::lex(&text);
                for token in tokens {
                    println!("{token:?}");
                }
            }
        }
    }
}
