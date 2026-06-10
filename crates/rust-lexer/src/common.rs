use std::borrow::Cow;
use std::hint::black_box;
use std::ops::Range;
use std::time::Duration;

use crate::lexers;

pub const EOF_BYTE: u8 = 0xff;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    LineComment,
    BlockComment,
    Whitespace,
    Frontmatter,
    Ident,
    InvalidIdent,
    RawIdent,
    UnknownPrefix,
    UnknownPrefixLifetime,
    Lifetime,
    RawLifetime,
    GuardedStrPrefix,

    Int,
    Float,
    Char,
    BChar,

    Str,
    BStr,
    CStr,

    RawStr,
    RawBStr,
    RawCStr,

    Semicolon,
    Comma,
    Dot,
    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,
    At,
    Hash,
    Tilde,
    Question,
    Colon,
    Dollar,
    Eq,
    Bang,
    Lt,
    Gt,
    Minus,
    And,
    Or,
    Plus,
    Star,
    Slash,
    Caret,
    Percent,
    Unknown,
    Eof,
}

impl From<lexers::rustc::TokenKind> for TokenKind {
    #[inline]
    fn from(rustc: lexers::rustc::TokenKind) -> Self {
        use crate::lexers::rustc::{LiteralKind, TokenKind as Rustc};

        match rustc {
            Rustc::LineComment { .. } => Self::LineComment,
            Rustc::BlockComment { .. } => Self::BlockComment,
            Rustc::Whitespace => Self::Whitespace,
            Rustc::Frontmatter { .. } => Self::Frontmatter,
            Rustc::Ident => Self::Ident,
            Rustc::InvalidIdent => Self::InvalidIdent,
            Rustc::RawIdent => Self::RawIdent,
            Rustc::UnknownPrefix => Self::UnknownPrefix,
            Rustc::UnknownPrefixLifetime => Self::UnknownPrefixLifetime,
            Rustc::RawLifetime => Self::RawLifetime,
            Rustc::GuardedStrPrefix => Self::GuardedStrPrefix,
            Rustc::Literal { kind, .. } => match kind {
                LiteralKind::Int { .. } => Self::Int,
                LiteralKind::Float { .. } => Self::Float,
                LiteralKind::Char { .. } => Self::Char,
                LiteralKind::Byte { .. } => Self::BChar,
                LiteralKind::Str { .. } => Self::Str,
                LiteralKind::ByteStr { .. } => Self::BStr,
                LiteralKind::CStr { .. } => Self::CStr,
                LiteralKind::RawStr { .. } => Self::RawStr,
                LiteralKind::RawByteStr { .. } => Self::RawBStr,
                LiteralKind::RawCStr { .. } => Self::RawCStr,
            },
            Rustc::Lifetime { .. } => Self::Lifetime,
            Rustc::Semi => Self::Semicolon,
            Rustc::Comma => Self::Comma,
            Rustc::Dot => Self::Dot,
            Rustc::OpenParen => Self::LParen,
            Rustc::CloseParen => Self::RParen,
            Rustc::OpenBrace => Self::LCurly,
            Rustc::CloseBrace => Self::RCurly,
            Rustc::OpenBracket => Self::LSquare,
            Rustc::CloseBracket => Self::RSquare,
            Rustc::At => Self::At,
            Rustc::Pound => Self::Hash,
            Rustc::Tilde => Self::Tilde,
            Rustc::Question => Self::Question,
            Rustc::Colon => Self::Colon,
            Rustc::Dollar => Self::Dollar,
            Rustc::Eq => Self::Eq,
            Rustc::Bang => Self::Bang,
            Rustc::Lt => Self::Lt,
            Rustc::Gt => Self::Gt,
            Rustc::Minus => Self::Minus,
            Rustc::And => Self::And,
            Rustc::Or => Self::Or,
            Rustc::Plus => Self::Plus,
            Rustc::Star => Self::Star,
            Rustc::Slash => Self::Slash,
            Rustc::Caret => Self::Caret,
            Rustc::Percent => Self::Percent,
            Rustc::Unknown => Self::Unknown,
            Rustc::Eof => Self::Eof,
        }
    }
}

fn pad(src: &[u8]) -> Vec<u8> {
    src.iter()
        .copied()
        .chain([EOF_BYTE; 64])
        .collect::<Vec<_>>()
}

pub trait Lexer {
    const NEEDS_PADDING: bool = true;

    fn lex_bytes(
        &self,
        bytes: &[u8],
        on_token: impl FnMut(TokenKind, *const u8, *const u8),
    ) -> Duration;

    fn pad_if_needed<'a>(&self, src: &'a [u8]) -> Cow<'a, [u8]> {
        if Self::NEEDS_PADDING {
            Cow::Owned(pad(src))
        } else {
            Cow::Borrowed(src)
        }
    }

    fn lex_str<'src>(
        &self,
        src: &'src str,
        mut on_token: impl FnMut(TokenKind, Range<usize>, &'src str),
    ) -> Duration {
        let bytes = self.pad_if_needed(src.as_bytes());
        let start = std::time::Instant::now();
        self.lex_bytes(&bytes, |kind, start_ptr, end_ptr| unsafe {
            let start_pos = start_ptr.offset_from_unsigned(bytes.as_ptr());
            let end_pos = end_ptr.offset_from_unsigned(bytes.as_ptr());
            let lexeme = &src[start_pos..end_pos];
            let range = start_pos..end_pos;
            on_token(kind, range, lexeme);
        });
        start.elapsed()
    }

    fn lex_str_to_vec(&self, str: &str) -> Duration {
        let bytes = self.pad_if_needed(str.as_bytes());
        let mut tokens: Vec<(TokenKind, u32)> = Vec::with_capacity(bytes.len());
        let mut token_ptr = tokens.as_mut_ptr();
        let duration = self.lex_bytes(&bytes, |kind, _, end_ptr| unsafe {
            let end_pos = end_ptr.offset_from_unsigned(bytes.as_ptr());
            token_ptr.write((kind, end_pos as u32));
            token_ptr = token_ptr.add(1);
        });
        drop(black_box(tokens));
        duration
    }
}

pub struct Rustc {}
impl Lexer for Rustc {
    const NEEDS_PADDING: bool = false;

    fn lex_str<'src>(
        &self,
        src: &'src str,
        mut on_token: impl FnMut(TokenKind, Range<usize>, &'src str),
    ) -> Duration {
        let start = std::time::Instant::now();
        let mut start_pos = 0usize;
        lexers::rustc::tokenize(src, lexers::rustc::FrontmatterAllowed::No).for_each(|token| {
            let end_pos = start_pos + token.len as usize;
            let range = start_pos..end_pos;
            let lexeme = &src[range.clone()];
            on_token(TokenKind::from(token.kind), range, lexeme);
            start_pos = end_pos;
        });
        start.elapsed()
    }

    fn lex_bytes(
        &self,
        bytes: &[u8],
        mut on_token: impl FnMut(TokenKind, *const u8, *const u8),
    ) -> Duration {
        let str = unsafe { std::str::from_utf8_unchecked(bytes) };
        self.lex_str(str, |kind, range, _| unsafe {
            let start_ptr = bytes.as_ptr().add(range.start);
            let end_ptr = bytes.as_ptr().add(range.start);
            on_token(kind, start_ptr, end_ptr);
        })
    }
}
