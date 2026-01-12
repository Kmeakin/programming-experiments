fn main() {
    let args = std::env::args();
    for arg in args {
        let text = match std::fs::read_to_string(&arg) {
            Err(err) => {
                eprintln!("Cannot read `{arg}`: {err}");
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
