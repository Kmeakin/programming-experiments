use std::borrow::Cow;
use std::ops::Range;

use crate::lexers;

pub const EOF_BYTE: u8 = 0xff;
pub const SIMD_PADDING: usize = 64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
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

    Semicolon = b';',
    Comma     = b',',
    Dot       = b'.',
    LParen    = b'(',
    RParen    = b')',
    LSquare   = b'[',
    RSquare   = b']',
    LCurly    = b'{',
    RCurly    = b'}',
    At        = b'@',
    Hash      = b'#',
    Tilde     = b'~',
    Question  = b'?',
    Colon     = b':',
    Dollar    = b'$',
    Eq        = b'=',
    Bang      = b'!',
    Lt        = b'<',
    Gt        = b'>',
    Minus     = b'-',
    And       = b'&',
    Or        = b'|',
    Plus      = b'+',
    Star      = b'*',
    Slash     = b'/',
    Caret     = b'^',
    Percent   = b'%',

    Unknown   = 0xfe,
    Eof       = 0xff,
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
        .chain([EOF_BYTE; SIMD_PADDING].iter().copied())
        .collect::<Vec<_>>()
}

pub trait Lexer {
    const NEEDS_PADDING: bool = true;

    fn lex_bytes<B>(
        &self,
        bytes: &[u8],
        acc: B,
        on_token: impl FnMut(B, TokenKind, *const u8, *const u8) -> B,
    ) -> B;

    fn pad_if_needed<'a>(&self, src: &'a [u8]) -> Cow<'a, [u8]> {
        if Self::NEEDS_PADDING {
            Cow::Owned(pad(src))
        } else {
            Cow::Borrowed(src)
        }
    }

    fn lex_str<'src, B>(
        &self,
        src: &'src str,
        acc: B,
        mut on_token: impl FnMut(B, TokenKind, Range<usize>, &'src str) -> B,
    ) -> B {
        let bytes = self.pad_if_needed(src.as_bytes());
        self.lex_bytes(&bytes, acc, |acc, kind, start_ptr, end_ptr| unsafe {
            let start_pos = start_ptr.offset_from_unsigned(bytes.as_ptr());
            let end_pos = end_ptr.offset_from_unsigned(bytes.as_ptr());
            let lexeme = &src[start_pos..end_pos];
            let range = start_pos..end_pos;
            on_token(acc, kind, range, lexeme)
        })
    }

    fn lex_str_to_vec(&self, str: &str, out: &mut Vec<(TokenKind, u32)>) {
        let bytes = self.pad_if_needed(str.as_bytes());
        debug_assert!(out.capacity() >= bytes.len());
        out.clear();
        let out_ptr = out.as_mut_ptr();
        let out_ptr = self.lex_bytes(&bytes, out_ptr, |out_ptr, kind, _, end| unsafe {
            let end_pos = end.offset_from_unsigned(bytes.as_ptr());
            out_ptr.write((kind, end_pos as u32));
            out_ptr.add(1)
        });
        unsafe {
            let len = out_ptr.offset_from_unsigned(out.as_mut_ptr());
            out.set_len(len);
        }
    }

    fn lex_str_to_soa(&self, str: &str, kinds_out: &mut Vec<TokenKind>, ends_out: &mut Vec<u32>) {
        let bytes = self.pad_if_needed(str.as_bytes());
        debug_assert!(kinds_out.capacity() >= bytes.len());
        debug_assert!(ends_out.capacity() >= bytes.len());

        kinds_out.clear();
        ends_out.clear();

        let kind_ptr = kinds_out.as_mut_ptr();
        let end_ptr = ends_out.as_mut_ptr();
        let (kind_ptr, end_ptr) = self.lex_bytes(
            &bytes,
            (kind_ptr, end_ptr),
            |(kind_ptr, end_ptr), kind, _, end| unsafe {
                let end_pos = end.offset_from_unsigned(bytes.as_ptr());
                kind_ptr.write(kind);
                end_ptr.write(end_pos as u32);
                (kind_ptr.add(1), end_ptr.add(1))
            },
        );

        unsafe {
            let kind_len = kind_ptr.offset_from_unsigned(kinds_out.as_mut_ptr());
            let end_len = end_ptr.offset_from_unsigned(ends_out.as_mut_ptr());
            debug_assert_eq!(kind_len, end_len);

            kinds_out.set_len(kind_len);
            ends_out.set_len(kind_len);
        }
    }
}

#[derive(Copy, Clone)]
pub struct Rustc {}
impl Lexer for Rustc {
    const NEEDS_PADDING: bool = false;

    fn lex_str<'src, B>(
        &self,
        src: &'src str,
        acc: B,
        mut on_token: impl FnMut(B, TokenKind, Range<usize>, &'src str) -> B,
    ) -> B {
        let (_, acc) = lexers::rustc::tokenize(src, lexers::rustc::FrontmatterAllowed::No).fold(
            (0usize, acc),
            |(start_pos, acc), token| {
                let end_pos = start_pos + token.len as usize;
                let range = start_pos..end_pos;
                let lexeme = &src[range.clone()];
                let start_pos = end_pos;
                let acc = on_token(acc, TokenKind::from(token.kind), range, lexeme);
                (start_pos, acc)
            },
        );
        acc
    }

    fn lex_bytes<B>(
        &self,
        bytes: &[u8],
        acc: B,
        mut on_token: impl FnMut(B, TokenKind, *const u8, *const u8) -> B,
    ) -> B {
        let str = unsafe { std::str::from_utf8_unchecked(bytes) };
        self.lex_str(str, acc, |acc, kind, range, _| unsafe {
            let start_ptr = bytes.as_ptr().add(range.start);
            let end_ptr = bytes.as_ptr().add(range.start);
            on_token(acc, kind, start_ptr, end_ptr)
        })
    }
}
