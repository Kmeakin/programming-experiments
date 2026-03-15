use logos::{Lexer, Logos};

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(Logos)]
pub enum TokenKind {
    #[regex(r"//[^\n]*", allow_greedy = true)] LineComment,
    #[token("/*", block_comment)] BlockComment,
    #[regex(r"[ \t\n]+")] Whitespace,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")] Ident,
    #[regex(r"r#[a-zA-Z_][a-zA-Z0-9_]*")] RawIdent,

    #[regex(r"(?i)[0-9][0-9_]*|0b[01_]*|0o[0-7_]*|0x[0-9a-f_]*", number)]
    Int,
    Float,

    #[regex(r"'[a-zA-Z0-9_]+")] Lifetime,
    #[regex(r#"'[^']'|'\\.'|'\\u\{[0-9a-fA-F]+}'|'\\x[0-9a-fA-F]{2}'"#)] Char,
    #[regex(r#"b'[^']'|b'\\.'|b'\\u\{[0-9a-fA-F]+}'|b'\\x[0-9a-fA-F]{2}'"#)] Byte,

    #[regex(r###"(?m)"([^"\\]|\\([ntrux0\n"'\\]))*""###)] Str,
    #[regex(r###"(?m)b"([^"\\]|\\([ntrux0\n"'\\]))*""###)] ByteStr,
    #[regex(r###"(?m)c"([^"\\]|\\([ntrux0\n"'\\]))*""###)] CStr,

    #[regex("r#*\"", raw_str)] RawStr,
    #[regex("br#*\"", raw_str)] RawByteStr,
    #[regex("cr#*\"", raw_str)] RawCStr,

    #[token(";")] Semi,
    #[token(",")] Comma,
    #[token(".")] Dot,
    #[token("(")] OpenParen,
    #[token(")")] CloseParen,
    #[token("{")] OpenBrace,
    #[token("}")] CloseBrace,
    #[token("[")] OpenBracket,
    #[token("]")] CloseBracket,
    #[token("@")] At,
    #[token("#")] Pound,
    #[token("~")] Tilde,
    #[token("?")] Question,
    #[token(":")] Colon,
    #[token("$")] Dollar,
    #[token("=")] Eq,
    #[token("!")] Bang,
    #[token("<")] Lt,
    #[token(">")] Gt,
    #[token("-")] Minus,
    #[token("&")] And,
    #[token("|")] Or,
    #[token("+")] Plus,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("^")] Caret,
    #[token("%")] Percent,
    Unknown,
}

fn block_comment(lexer: &mut Lexer<TokenKind>) {
    let mut depth = 1;
    loop {
        let remainder = lexer.remainder();
        if remainder.is_empty() {
            break;
        }

        if remainder.starts_with("/*") {
            lexer.bump(2);
            depth += 1;
            continue;
        }

        if remainder.starts_with("*/") {
            lexer.bump(2);
            depth -= 1;
            if depth == 0 {
                break;
            }
            continue;
        }

        let char = remainder.chars().next().unwrap();
        lexer.bump(char.len_utf8());
    }
}

fn raw_str(lexer: &mut Lexer<TokenKind>) {
    let mut num_hashes = 0;
    let mut slice = lexer.slice();

    if slice.starts_with("br") {
        slice = &slice[2..];
    } else {
        assert!(lexer.slice().starts_with('r'));
        slice = &slice[1..];
    }

    while slice.starts_with('#') {
        slice = &slice[1..];
        num_hashes += 1;
    }

    while let Some(pos) = memchr::memchr(b'"', lexer.remainder().as_bytes()) {
        lexer.bump(pos);
        lexer.bump(1);

        let mut num_hashes = num_hashes;
        while lexer.remainder().starts_with('#') {
            lexer.bump(1);
            num_hashes -= 1;
        }
        if num_hashes == 0 {
            return;
        }
    }
}

fn number(lexer: &mut Lexer<TokenKind>) -> TokenKind {
    let kind = match lexer.remainder().as_bytes() {
        [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => TokenKind::Int,
        [b'.', ..] => {
            lexer.bump(1);
            while lexer
                .remainder()
                .starts_with(|c: char| c.is_ascii_digit() || c == '_')
            {
                lexer.bump(1);
            }
            if lexer.remainder().starts_with(['e', 'E']) {
                lexer.bump(1);
                if lexer.remainder().starts_with(['+', '-']) {
                    lexer.bump(1);
                }
                while lexer
                    .remainder()
                    .starts_with(|c: char| c.is_ascii_digit() || c == '_')
                {
                    lexer.bump(1);
                }
            }

            TokenKind::Float
        }
        [b'e' | b'E', ..] => {
            lexer.bump(1);
            if lexer.remainder().starts_with(['+', '-']) {
                lexer.bump(1);
            }
            while lexer
                .remainder()
                .starts_with(|c: char| c.is_ascii_digit() || c == '_')
            {
                lexer.bump(1);
            }
            TokenKind::Float
        }
        _ => TokenKind::Int,
    };
    while lexer
        .remainder()
        .starts_with(|c: char| c.is_ascii_alphanumeric() || c == '_')
    {
        lexer.bump(1);
    }
    kind
}

#[allow(clippy::cast_possible_truncation)]
pub fn lex_iter(input: &str) -> impl Iterator<Item = (TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    Lexer::new(input).spanned().map(|(kind, span)| {
        let kind = match kind {
            Ok(kind) => kind,
            Err(()) => TokenKind::Unknown,
        };
        let len = span.end - span.start;
        (kind, len as u32)
    })
}
