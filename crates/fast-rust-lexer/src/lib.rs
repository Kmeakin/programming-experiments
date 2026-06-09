#![feature(
    bstr,
    explicit_tail_calls,
    iter_next_chunk,
    likely_unlikely,
    link_llvm_intrinsics,
    pointer_is_aligned_to,
    portable_simd,
    rust_preserve_none_cc,
    simd_ffi,
    slice_from_ptr_range
)]
#![allow(
    clippy::inline_always,
    incomplete_features,
    internal_features,
    unused_features
)]

pub mod impls;
pub mod utils;

pub use crate::impls::*;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TokenKind {
    Bang      = b'!',
    Hash      = b'#',
    Dollar    = b'$',
    Percent   = b'%',
    Ampersand = b'&',
    LParen    = b'(',
    RParen    = b')',
    Star      = b'*',
    Plus      = b'+',
    Comma     = b',',
    Minus     = b'-',
    Dot       = b'.',
    Slash     = b'/',
    Colon     = b':',
    Semicolon = b';',
    Lt        = b'<',
    Eq        = b'=',
    Gt        = b'>',
    Question  = b'?',
    At        = b'@',
    LSquare   = b'[',
    RSquare   = b']',
    Caret     = b'^',
    LCurly    = b'{',
    Bar       = b'|',
    RCurly    = b'}',
    Tilde     = b'~',
    // Not valid punctuation, but we can use the same byte for simplicity
    Backslash = b'\\',
    Backquote = b'`',

    Whitespace,
    LineComment,
    BlockComment,

    Ident,
    RawIdent,
    Int,
    Float,

    Str,
    RawStr,
    CStr,
    RawCStr,
    ByteStr,
    RawByteStr,

    HashStr,
    HashByteStr,
    HashCStr,

    GuardedStr,

    Char,
    Byte,
    Lifetime,
    RawLifetime,

    Unknown,
    Eof       = 0xff,
}

impl TokenKind {
    pub fn from_u8(b: u8) -> Option<Self> {
        if b == Self::Whitespace as u8 {
            return Some(Self::Whitespace);
        }
        if b == Self::LineComment as u8 {
            return Some(Self::LineComment);
        }
        if b == Self::BlockComment as u8 {
            return Some(Self::BlockComment);
        }

        if b == Self::Ident as u8 {
            return Some(Self::Ident);
        }
        if b == Self::RawIdent as u8 {
            return Some(Self::RawIdent);
        }
        if b == Self::Int as u8 {
            return Some(Self::Int);
        }
        if b == Self::Float as u8 {
            return Some(Self::Float);
        }

        if b == Self::Str as u8 {
            return Some(Self::Str);
        }
        if b == Self::RawStr as u8 {
            return Some(Self::RawStr);
        }
        if b == Self::CStr as u8 {
            return Some(Self::CStr);
        }
        if b == Self::RawCStr as u8 {
            return Some(Self::RawCStr);
        }
        if b == Self::ByteStr as u8 {
            return Some(Self::ByteStr);
        }
        if b == Self::RawByteStr as u8 {
            return Some(Self::RawByteStr);
        }

        if b == Self::Char as u8 {
            return Some(Self::Char);
        }
        if b == Self::Byte as u8 {
            return Some(Self::Byte);
        }
        if b == Self::Lifetime as u8 {
            return Some(Self::Lifetime);
        }
        if b == Self::RawLifetime as u8 {
            return Some(Self::RawLifetime);
        }

        if b == Self::Unknown as u8 {
            return Some(Self::Unknown);
        }

        Some(match b {
            b'!' => Self::Bang,
            b'#' => Self::Hash,
            b'$' => Self::Dollar,
            b'%' => Self::Percent,
            b'&' => Self::Ampersand,
            b'(' => Self::LParen,
            b')' => Self::RParen,
            b'*' => Self::Star,
            b'+' => Self::Plus,
            b',' => Self::Comma,
            b'-' => Self::Minus,
            b'.' => Self::Dot,
            b'/' => Self::Slash,
            b':' => Self::Colon,
            b';' => Self::Semicolon,
            b'<' => Self::Lt,
            b'=' => Self::Eq,
            b'>' => Self::Gt,
            b'?' => Self::Question,
            b'@' => Self::At,
            b'[' => Self::LSquare,
            b']' => Self::RSquare,
            b'^' => Self::Caret,
            b'{' => Self::LCurly,
            b'|' => Self::Bar,
            b'}' => Self::RCurly,
            b'~' => Self::Tilde,
            b'\\' => Self::Backslash,
            b'`' => Self::Backquote,
            _ => return None,
        })
    }

    pub fn is_punct(self) -> bool {
        matches!(
            self,
            Self::Bang
                | Self::Hash
                | Self::Dollar
                | Self::Percent
                | Self::Ampersand
                | Self::LParen
                | Self::RParen
                | Self::Star
                | Self::Plus
                | Self::Comma
                | Self::Minus
                | Self::Dot
                | Self::Slash
                | Self::Colon
                | Self::Semicolon
                | Self::Lt
                | Self::Eq
                | Self::Gt
                | Self::Question
                | Self::At
                | Self::LSquare
                | Self::RSquare
                | Self::Caret
                | Self::LCurly
                | Self::Bar
                | Self::RCurly
                | Self::Tilde
                | Self::Backslash
                | Self::Backquote
        )
    }
}
