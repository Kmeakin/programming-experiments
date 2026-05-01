#![feature(
    explicit_tail_calls,
    link_llvm_intrinsics,
    portable_simd,
    rust_preserve_none_cc,
    simd_ffi,
    slice_from_ptr_range
)]
#![cfg_attr(test, feature(iter_next_chunk, bstr))]
#![allow(incomplete_features, internal_features)]

pub mod impls;
pub mod utils;

pub use crate::impls::*;

#[cfg(test)]
mod tests;

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
