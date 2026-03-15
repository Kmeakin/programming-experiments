pub mod logos;
pub mod manual;
pub mod rustc;

#[cfg(test)]
mod tests;

pub fn rustc_lex_iter(input: &str) -> impl Iterator<Item = (TokenKind, u32)> {
    rustc::tokenize(input, rustc::FrontmatterAllowed::No)
        .map(|token| (TokenKind::from(token.kind), token.len))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    /// A line comment, e.g. `// comment`.
    LineComment,

    /// A block comment, e.g. `/* block comment */`.
    ///
    /// Block comments can be recursive, so a sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment,

    /// Any whitespace character sequence.
    Whitespace,

    Frontmatter,

    /// An identifier or keyword, e.g. `ident` or `continue`.
    Ident,

    /// An identifier that is invalid because it contains emoji.
    InvalidIdent,

    /// A raw identifier, e.g. "r#ident".
    RawIdent,

    /// An unknown literal prefix, like `foo#`, `foo'`, `foo"`. Excludes
    /// literal prefixes that contain emoji, which are considered "invalid".
    ///
    /// Note that only the
    /// prefix (`foo`) is included in the token, not the separator (which is
    /// lexed as its own distinct token). In Rust 2021 and later, reserved
    /// prefixes are reported as errors; in earlier editions, they result in a
    /// (allowed by default) lint, and are treated as regular identifier
    /// tokens.
    UnknownPrefix,

    /// An unknown prefix in a lifetime, like `'foo#`.
    ///
    /// Like `UnknownPrefix`, only the `'` and prefix are included in the token
    /// and not the separator.
    UnknownPrefixLifetime,

    /// A raw lifetime, e.g. `'r#foo`. In edition < 2021 it will be split into
    /// several tokens: `'r` and `#` and `foo`.
    RawLifetime,

    /// Guarded string literal prefix: `#"` or `##`.
    ///
    /// Used for reserving "guarded strings" (RFC 3598) in edition 2024.
    /// Split into the component tokens on older editions.
    GuardedStrPrefix,

    // Literals, e.g. `12u8`, `1.0e-40`, `b"123"`. Note that `_` is an invalid
    // suffix, but may be present here on string and float literals. Users of
    // this type will need to check for and reject that case.
    /// `12_u8`, `0o100`, `0b120i99`, `1f32`.
    Int,
    /// `12.34f32`, `1e3`, but not `1f32`.
    Float,
    /// `'a'`, `'\\'`, `'''`, `';`
    Char,
    /// `b'a'`, `b'\\'`, `b'''`, `b';`
    Byte,
    /// `"abc"`, `"abc`
    Str,
    /// `b"abc"`, `b"abc`
    ByteStr,
    /// `c"abc"`, `c"abc`
    CStr,
    /// `r"abc"`, `r#"abc"#`, `r####"ab"###"c"####`, `r#"a`. `None` indicates
    /// an invalid literal.
    RawStr,
    /// `br"abc"`, `br#"abc"#`, `br####"ab"###"c"####`, `br#"a`. `None`
    /// indicates an invalid literal.
    RawByteStr,
    /// `cr"abc"`, "cr#"abc"#", `cr#"a`. `None` indicates an invalid literal.
    RawCStr,

    /// A lifetime, e.g. `'a`.
    Lifetime,

    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `@`
    At,
    /// `#`
    Hash,
    /// `~`
    Tilde,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `!`
    Bang,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,

    /// End of input.
    Eof,
}

impl From<rustc::TokenKind> for TokenKind {
    fn from(value: rustc::TokenKind) -> Self {
        match value {
            rustc::TokenKind::LineComment { .. } => Self::LineComment,
            rustc::TokenKind::BlockComment { .. } => Self::BlockComment,
            rustc::TokenKind::Whitespace => Self::Whitespace,
            rustc::TokenKind::Ident => Self::Ident,
            rustc::TokenKind::RawIdent => Self::RawIdent,
            rustc::TokenKind::Literal { kind, .. } => match kind {
                rustc::LiteralKind::Int { .. } => Self::Int,
                rustc::LiteralKind::Float { .. } => Self::Float,
                rustc::LiteralKind::Char { .. } => Self::Char,
                rustc::LiteralKind::Byte { .. } => Self::Byte,
                rustc::LiteralKind::Str { .. } => Self::Str,
                rustc::LiteralKind::ByteStr { .. } => Self::ByteStr,
                rustc::LiteralKind::RawStr { .. } => Self::RawStr,
                rustc::LiteralKind::RawByteStr { .. } => Self::RawByteStr,
                rustc::LiteralKind::CStr { .. } => Self::CStr,
                rustc::LiteralKind::RawCStr { .. } => Self::RawCStr,
            },
            rustc::TokenKind::Lifetime { .. } => Self::Lifetime,
            rustc::TokenKind::Semi => Self::Semi,
            rustc::TokenKind::Comma => Self::Comma,
            rustc::TokenKind::Dot => Self::Dot,
            rustc::TokenKind::OpenParen => Self::OpenParen,
            rustc::TokenKind::CloseParen => Self::CloseParen,
            rustc::TokenKind::OpenBrace => Self::OpenBrace,
            rustc::TokenKind::CloseBrace => Self::CloseBrace,
            rustc::TokenKind::OpenBracket => Self::OpenBracket,
            rustc::TokenKind::CloseBracket => Self::CloseBracket,
            rustc::TokenKind::At => Self::At,
            rustc::TokenKind::Pound => Self::Hash,
            rustc::TokenKind::Tilde => Self::Tilde,
            rustc::TokenKind::Question => Self::Question,
            rustc::TokenKind::Colon => Self::Colon,
            rustc::TokenKind::Dollar => Self::Dollar,
            rustc::TokenKind::Eq => Self::Eq,
            rustc::TokenKind::Bang => Self::Bang,
            rustc::TokenKind::Lt => Self::Lt,
            rustc::TokenKind::Gt => Self::Gt,
            rustc::TokenKind::Minus => Self::Minus,
            rustc::TokenKind::And => Self::And,
            rustc::TokenKind::Or => Self::Or,
            rustc::TokenKind::Plus => Self::Plus,
            rustc::TokenKind::Star => Self::Star,
            rustc::TokenKind::Slash => Self::Slash,
            rustc::TokenKind::Caret => Self::Caret,
            rustc::TokenKind::Percent => Self::Percent,
            rustc::TokenKind::Unknown => Self::Unknown,
            rustc::TokenKind::Frontmatter { .. } => Self::Frontmatter,
            rustc::TokenKind::InvalidIdent => todo!(),
            rustc::TokenKind::UnknownPrefix => todo!(),
            rustc::TokenKind::UnknownPrefixLifetime => todo!(),
            rustc::TokenKind::RawLifetime => todo!(),
            rustc::TokenKind::GuardedStrPrefix => todo!(),
            rustc::TokenKind::Eof => todo!(),
        }
    }
}

impl From<logos::TokenKind> for TokenKind {
    fn from(value: logos::TokenKind) -> Self {
        match value {
            logos::TokenKind::LineComment => Self::LineComment,
            logos::TokenKind::BlockComment => Self::BlockComment,
            logos::TokenKind::Whitespace => Self::Whitespace,
            logos::TokenKind::Ident => Self::Ident,
            logos::TokenKind::RawIdent => Self::RawIdent,
            logos::TokenKind::Int => Self::Int,
            logos::TokenKind::Float => Self::Float,
            logos::TokenKind::Char => Self::Char,
            logos::TokenKind::Byte => Self::Byte,
            logos::TokenKind::Str => Self::Str,
            logos::TokenKind::ByteStr => Self::ByteStr,
            logos::TokenKind::RawStr => Self::RawStr,
            logos::TokenKind::RawByteStr => Self::RawByteStr,
            logos::TokenKind::RawCStr => Self::RawCStr,
            logos::TokenKind::CStr => Self::CStr,
            logos::TokenKind::Lifetime => Self::Lifetime,
            logos::TokenKind::Semi => Self::Semi,
            logos::TokenKind::Comma => Self::Comma,
            logos::TokenKind::Dot => Self::Dot,
            logos::TokenKind::OpenParen => Self::OpenParen,
            logos::TokenKind::CloseParen => Self::CloseParen,
            logos::TokenKind::OpenBrace => Self::OpenBrace,
            logos::TokenKind::CloseBrace => Self::CloseBrace,
            logos::TokenKind::OpenBracket => Self::OpenBracket,
            logos::TokenKind::CloseBracket => Self::CloseBracket,
            logos::TokenKind::At => Self::At,
            logos::TokenKind::Pound => Self::Hash,
            logos::TokenKind::Tilde => Self::Tilde,
            logos::TokenKind::Question => Self::Question,
            logos::TokenKind::Colon => Self::Colon,
            logos::TokenKind::Dollar => Self::Dollar,
            logos::TokenKind::Eq => Self::Eq,
            logos::TokenKind::Bang => Self::Bang,
            logos::TokenKind::Lt => Self::Lt,
            logos::TokenKind::Gt => Self::Gt,
            logos::TokenKind::Minus => Self::Minus,
            logos::TokenKind::And => Self::And,
            logos::TokenKind::Or => Self::Or,
            logos::TokenKind::Plus => Self::Plus,
            logos::TokenKind::Star => Self::Star,
            logos::TokenKind::Slash => Self::Slash,
            logos::TokenKind::Caret => Self::Caret,
            logos::TokenKind::Percent => Self::Percent,
            logos::TokenKind::Unknown => Self::Unknown,
        }
    }
}
