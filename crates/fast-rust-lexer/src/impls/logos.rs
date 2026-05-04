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
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LSquare,
    #[token("]")] RSquare,
    #[token("{")] LCurly,
    #[token("}")] RCurly,
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
pub fn lex_iter(input: &str) -> impl Iterator<Item = (crate::TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    Lexer::new(input).spanned().map(|(kind, span)| {
        let kind = match kind {
            Ok(kind) => kind,
            Err(()) => TokenKind::Unknown,
        };
        let len = span.end - span.start;
        (crate::TokenKind::from(kind), len as u32)
    })
}

impl From<TokenKind> for crate::TokenKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::LineComment => Self::LineComment,
            TokenKind::BlockComment => Self::BlockComment,
            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::Ident => Self::Ident,
            TokenKind::RawIdent => Self::RawIdent,
            TokenKind::Int => Self::Int,
            TokenKind::Float => Self::Float,
            TokenKind::Char => Self::Char,
            TokenKind::Byte => Self::Byte,
            TokenKind::Str => Self::Str,
            TokenKind::ByteStr => Self::ByteStr,
            TokenKind::RawStr => Self::RawStr,
            TokenKind::RawByteStr => Self::RawByteStr,
            TokenKind::RawCStr => Self::RawCStr,
            TokenKind::CStr => Self::CStr,
            TokenKind::Lifetime => Self::Lifetime,
            TokenKind::Semi => Self::Semicolon,
            TokenKind::Comma => Self::Comma,
            TokenKind::Dot => Self::Dot,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,
            TokenKind::LSquare => Self::LSquare,
            TokenKind::RSquare => Self::RSquare,
            TokenKind::LCurly => Self::LCurly,
            TokenKind::RCurly => Self::RCurly,
            TokenKind::At => Self::At,
            TokenKind::Pound => Self::Hash,
            TokenKind::Tilde => Self::Tilde,
            TokenKind::Question => Self::Question,
            TokenKind::Colon => Self::Colon,
            TokenKind::Dollar => Self::Dollar,
            TokenKind::Eq => Self::Eq,
            TokenKind::Bang => Self::Bang,
            TokenKind::Lt => Self::Lt,
            TokenKind::Gt => Self::Gt,
            TokenKind::Minus => Self::Minus,
            TokenKind::And => Self::Ampersand,
            TokenKind::Or => Self::Bar,
            TokenKind::Plus => Self::Plus,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Caret => Self::Caret,
            TokenKind::Percent => Self::Percent,
            TokenKind::Unknown => Self::Unknown,
        }
    }
}
