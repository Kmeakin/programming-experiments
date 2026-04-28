use std::bstr::ByteStr;
use std::fmt;
use std::ops::{Shl, ShlAssign};
use std::simd::prelude::*;

use crate::utils::simdx::movemask;

pub const EOF_BYTE: u8 = 0xFF;

fn eq<const N: usize>(vec: Simd<u8, N>, byte: u8) -> Mask<i8, N> { vec.simd_eq(Simd::splat(byte)) }

fn in_range<const N: usize>(vec: Simd<u8, N>, min: u8, max: u8) -> Mask<i8, N> {
    Simd::splat(min).simd_le(vec) & vec.simd_le(Simd::splat(max))
}

fn write_and_advance<T>(out: *mut u8, val: T) -> *mut u8 {
    unsafe {
        out.cast::<T>().write_unaligned(val);
        out.add(size_of::<T>())
    }
}

fn write_token(out: *mut u8, kind: TokenKind, len: u32) -> *mut u8 {
    let out = write_and_advance(out, kind as u8);
    write_and_advance(out, len)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

    Char,
    Byte,
    Lifetime,
    RawLifetime,

    Unknown,
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

#[derive(Copy, Clone)]
struct BitString<const N: usize> {
    bits: u64,
}

impl<const BITS: usize> BitString<BITS> {
    pub fn new(bits: u64) -> Self { Self { bits } }
    pub fn leading_zeros(self) -> usize { Ord::min(self.bits.leading_zeros() as usize, BITS) }
    pub fn leading_ones(self) -> usize { Ord::min(self.bits.leading_ones() as usize, BITS) }

    fn any(self) -> bool {
        match BITS {
            16 => (self.bits >> 48) as u16 != 0,
            32 => (self.bits >> 32) as u32 != 0,
            64 => self.bits != 0,
            _ => unreachable!(),
        }
    }
}

impl<const BITS: usize> fmt::Debug for BitString<BITS> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match BITS {
            16 => write!(f, "{:0BITS$b}", (self.bits >> 48) as u16),
            32 => write!(f, "{:0BITS$b}", (self.bits >> 32) as u32),
            64 => write!(f, "{:0BITS$b}", self.bits),
            _ => unreachable!(),
        }
    }
}

impl<const BITS: usize> Shl<usize> for BitString<BITS> {
    type Output = Self;
    fn shl(self, amount: usize) -> Self::Output {
        debug_assert!(amount <= BITS, "amount = {amount}, BITS = {BITS}");
        Self {
            bits: self.bits << amount,
        }
    }
}

impl<const BITS: usize> ShlAssign<usize> for BitString<BITS> {
    fn shl_assign(&mut self, amount: usize) { *self = *self << amount; }
}

struct Chunk<const VEC_LEN: usize> {
    vec_start_ptr: *const u8,
    bytes:         Simd<u8, VEC_LEN>,
    remainder:     usize,
    whitespace:    BitString<VEC_LEN>,
    newline:       BitString<VEC_LEN>,
    slash_slash:   BitString<VEC_LEN>,
    slash_star:    BitString<VEC_LEN>,
    star_slash:    BitString<VEC_LEN>,
    ident:         BitString<VEC_LEN>,
    quote:         BitString<VEC_LEN>,
    apostrophe:    BitString<VEC_LEN>,
    punctuation:   BitString<VEC_LEN>,
}

impl<const VEC_LEN: usize> Chunk<VEC_LEN> {
    fn load(ptr: *const u8) -> Self {
        unsafe {
            let bytes = ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let byte1 = ptr.add(VEC_LEN + 1).read();

            let newline = movemask(eq(bytes, b'\n')).reverse_bits();
            let whitespace = movemask(eq(bytes, b' ') | eq(bytes, b'\t')).reverse_bits() | newline;

            let slash = movemask(eq(bytes, b'/')).reverse_bits();
            let star = movemask(eq(bytes, b'*')).reverse_bits();
            let slash1 = slash << 1 | u64::from(byte1 == b'/');
            let star1 = star << 1 | u64::from(byte1 == b'*');

            let slash_slash = slash & slash1;
            let slash_star = slash & star1;
            let star_slash = star & slash1;

            let ident = movemask(
                eq(bytes, b'_')
                    | in_range(bytes, b'a', b'z')
                    | in_range(bytes, b'A', b'Z')
                    | in_range(bytes, b'0', b'9'),
            )
            .reverse_bits();

            let quote = movemask(eq(bytes, b'"')).reverse_bits();
            let apostrophe = movemask(eq(bytes, b'\'')).reverse_bits();

            let printable = movemask(in_range(bytes, b'!', b'~')).reverse_bits();
            let punctuation =
                printable & !(whitespace | slash_slash | slash_star | ident | quote | apostrophe);

            Self {
                vec_start_ptr: (ptr),
                bytes:         (bytes),
                remainder:     VEC_LEN,
                whitespace:    BitString::new(whitespace),
                newline:       BitString::new(newline),
                slash_slash:   BitString::new(slash_slash),
                slash_star:    BitString::new(slash_star),
                star_slash:    BitString::new(star_slash),
                ident:         BitString::new(ident),
                quote:         BitString::new(quote),
                apostrophe:    BitString::new(apostrophe),
                punctuation:   BitString::new(punctuation),
            }
        }
    }

    fn advance(&mut self, amount: usize) {
        debug_assert!(
            amount <= self.remainder,
            "amount = {amount}, remainder = {}",
            self.remainder
        );

        self.remainder -= amount;
        self.whitespace <<= amount;
        self.newline <<= amount;
        self.slash_slash <<= amount;
        self.slash_star <<= amount;
        self.star_slash <<= amount;
        self.ident <<= amount;
        self.quote <<= amount;
        self.apostrophe <<= amount;
        self.punctuation <<= amount;
    }

    fn ptr(&self) -> *const u8 { unsafe { self.vec_start_ptr.add(VEC_LEN - self.remainder) } }
}

impl<const VEC_LEN: usize> fmt::Debug for Chunk<VEC_LEN> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chunk")
            .field("vec_ptr\t", &self.vec_start_ptr)
            .field(
                "bytes\t",
                &ByteStr::new(&self.bytes[VEC_LEN - self.remainder..]),
            )
            .field("remainder\t", &self.remainder)
            .field("whitespace\t", &self.whitespace)
            .field("newline\t", &self.newline)
            .field("slash_slash\t", &self.slash_slash)
            .field("slash_star\t", &self.slash_star)
            .field("star_slash\t", &self.star_slash)
            .field("ident\t", &self.ident)
            .field("quote\t", &self.quote)
            .field("apostrophe\t", &self.apostrophe)
            .field("punctuation\t", &self.punctuation)
            .finish()
    }
}

pub fn lex<'a, const VEC_LEN: usize>(input: &[u8], out_slice: &'a mut [u8]) -> &'a mut [u8] {
    debug_assert!(u32::try_from(input.len()).is_ok());
    debug_assert!(input.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
    debug_assert!(out_slice.len() >= input.len());

    unsafe {
        let std::ops::Range { start, end } = input.as_ptr_range();
        let src_end = end.sub(VEC_LEN * 2);
        let src_ptr = start;
        let out_ptr = out_slice.as_mut_ptr();

        let chunk = Chunk::<VEC_LEN>::load(src_ptr);
        let (_src_ptr, out_ptr) = lex_inner::<VEC_LEN>(chunk, src_end, out_ptr);

        let out_len = out_ptr.offset_from_unsigned(out_slice.as_mut_ptr());
        &mut out_slice[..out_len]
    }
}

fn lex_inner<const VEC_LEN: usize>(
    chunk: Chunk<VEC_LEN>,
    src_end: *const u8,
    out_ptr: *mut u8,
) -> (Chunk<VEC_LEN>, *mut u8) {
    dbg!(&chunk);
    if chunk.whitespace.leading_ones() > 0 {
        become lex_whitespace(chunk, src_end, out_ptr);
    }

    if chunk.punctuation.leading_ones() > 0 {
        become lex_punctuation(chunk, src_end, out_ptr);
    }

    if chunk.slash_slash.leading_ones() > 0 {
        become lex_line_comment(chunk, src_end, out_ptr);
    }

    #[cfg(false)]
    if chunk.slash_star.leading_ones() > 0 {
        become lex_block_comment(chunk, src_end, out_ptr);
    }

    if chunk.ident.leading_ones() > 0 {
        become lex_ident(chunk, src_end, out_ptr);
    }

    let eof = BitString::<VEC_LEN>::new(movemask(eq(chunk.bytes, EOF_BYTE)).reverse_bits());
    if eof.any() {
        return (chunk, out_ptr);
    }

    todo!()
}

fn lex_whitespace<const VEC_LEN: usize>(
    mut chunk: Chunk<VEC_LEN>,
    src_end: *const u8,
    mut out_ptr: *mut u8,
) -> (Chunk<VEC_LEN>, *mut u8) {
    dbg!(&chunk);
    let mut total_len = chunk.whitespace.leading_ones();

    if total_len != chunk.remainder {
        chunk.advance(total_len);
        out_ptr = write_token(out_ptr, TokenKind::Whitespace, total_len as u32);
        become lex_inner(chunk, src_end, out_ptr);
    }

    unsafe {
        let mut src_ptr = chunk.vec_start_ptr.add(VEC_LEN);
        loop {
            let vec = src_ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let whitespace = BitString::<VEC_LEN>::new(
                movemask(eq(vec, b' ') | eq(vec, b'\t') | eq(vec, b'\n')).reverse_bits(),
            );
            let len = whitespace.leading_ones();
            total_len += len;
            if len != VEC_LEN {
                out_ptr = write_token(out_ptr, TokenKind::Whitespace, total_len as u32);
                chunk = Chunk::<VEC_LEN>::load(src_ptr);
                chunk.advance(len);
                become lex_inner(chunk, src_end, out_ptr);
            }
            src_ptr = src_ptr.add(VEC_LEN);
        }
    }
}

fn lex_punctuation<const VEC_LEN: usize>(
    mut chunk: Chunk<VEC_LEN>,
    src_end: *const u8,
    mut out_ptr: *mut u8,
) -> (Chunk<VEC_LEN>, *mut u8) {
    unsafe {
        let src = chunk.vec_start_ptr.add(VEC_LEN - chunk.remainder);
        let total_len = chunk.punctuation.leading_ones();

        if total_len == chunk.remainder {
            let ptr = chunk.vec_start_ptr.add(VEC_LEN);
            chunk = Chunk::<VEC_LEN>::load(ptr);
        } else {
            chunk.advance(total_len);
        }

        std::ptr::copy_nonoverlapping(src, out_ptr, total_len);
        out_ptr = out_ptr.add(total_len);
        become lex_inner(chunk, src_end, out_ptr);
    }
}

fn lex_line_comment<const VEC_LEN: usize>(
    mut chunk: Chunk<VEC_LEN>,
    src_end: *const u8,
    mut out_ptr: *mut u8,
) -> (Chunk<VEC_LEN>, *mut u8) {
    unsafe {
        let token_start = chunk.vec_start_ptr;
        let len = chunk.newline.leading_zeros();

        if len < chunk.remainder {
            chunk.advance(len);
            let token_end = chunk.ptr();
            let len = token_end.offset_from_unsigned(token_start);
            out_ptr = write_token(out_ptr, TokenKind::LineComment, len as u32);
            become lex_inner(chunk, token_end, out_ptr);
        }

        let mut src_ptr = chunk.vec_start_ptr;
        loop {
            src_ptr = src_ptr.add(VEC_LEN);
            if src_ptr >= src_end {
                let len = src_end.offset_from_unsigned(token_start);
                out_ptr = write_token(out_ptr, TokenKind::LineComment, len as u32);
                return (chunk, out_ptr);
            }

            let vec = src_ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let newline = BitString::<VEC_LEN>::new(movemask(eq(vec, b'\n')).reverse_bits());
            let len = newline.leading_zeros();
            if len < VEC_LEN {
                let token_end = src_ptr.add(len);
                let token_len = token_end.offset_from_unsigned(token_start);
                out_ptr = write_token(out_ptr, TokenKind::LineComment, token_len as u32);
                chunk = Chunk::<VEC_LEN>::load(src_ptr);
                chunk.advance(len);
                become lex_inner(chunk, src_end, out_ptr);
            }
        }
    }
}

#[cfg(false)]
fn lex_block_comment<const VEC_LEN: usize>(
    mut chunk: Chunk<VEC_LEN>,
    src_end: *const u8,
    mut out_ptr: *mut u8,
) -> (Chunk<VEC_LEN>, *mut u8) {
    dbg!(&chunk);

    let token_start = chunk.ptr();
    chunk.advance(2);
    let mut depth = 1u32;

    unsafe {
        while chunk.remainder > 0 {
            dbg!(&chunk);
            dbg!(depth);

            let open_len = chunk.slash_star.leading_zeros();
            let close_len = chunk.star_slash.leading_zeros();
            if close_len < open_len {
                chunk.advance(close_len);
                chunk.advance(2);
                depth -= 1;
                if depth == 0 {
                    let token_end = chunk.ptr();
                    debug_assert_eq!(token_end.sub(2).read(), b'*');
                    debug_assert_eq!(token_end.sub(1).read(), b'/');

                    let token_len = token_end.offset_from_unsigned(token_start);
                    out_ptr = write_token(out_ptr, TokenKind::BlockComment, token_len as u32);
                    become lex_inner(chunk, src_end, out_ptr);
                }
                continue;
            }

            if open_len < chunk.remainder {
                depth += 1;
                chunk.advance(open_len);
                chunk.advance(2);
                let ptr = chunk.ptr();
                debug_assert_eq!(ptr.sub(2).read(), b'/');
                debug_assert_eq!(ptr.sub(1).read(), b'*');
                continue;
            }

            break;
        }

        dbg!(&chunk);
        dbg!(depth);

        let mut cur_ptr = chunk.vec_start_ptr;
        loop {
            cur_ptr = cur_ptr.add(VEC_LEN);
            if cur_ptr >= src_end {
                let token_len = src_end.offset_from_unsigned(token_start);
                out_ptr = write_token(out_ptr, TokenKind::BlockComment, token_len as u32);
                return (chunk, out_ptr);
            }
            let vec = cur_ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let byte1 = cur_ptr.add(1).read();
            let slash = movemask(eq(vec, b'/')).reverse_bits();
            let star = movemask(eq(vec, b'*')).reverse_bits();
            let slash1 = slash << 1 | u64::from(byte1 == b'/');
            let star1 = star << 1 | u64::from(byte1 == b'*');
            let mut open = BitString::<VEC_LEN>::new(slash & star1);
            let mut close = BitString::<VEC_LEN>::new(star & slash1);
            let mut remainder = VEC_LEN;
            while remainder > 0 {
                dbg!(ByteStr::new(&vec));
                dbg!(open);
                dbg!(close);
                dbg!(depth);

                let open_len = open.leading_zeros();
                let close_len = close.leading_zeros();
                if close_len < open_len {
                    open <<= close_len;
                    close <<= close_len;
                    remainder -= close_len;

                    open <<= 2;
                    close <<= 2;
                    remainder -= 2;

                    depth -= 1;
                    if depth == 0 {
                        chunk = Chunk::<VEC_LEN>::load(cur_ptr);
                        dbg!(&chunk);
                        chunk.advance(VEC_LEN - remainder);
                        dbg!(&chunk);
                        let token_end = chunk.ptr();
                        let token_len = token_end.offset_from_unsigned(token_start);
                        out_ptr = write_token(out_ptr, TokenKind::BlockComment, token_len as u32);
                        become lex_inner(chunk, src_end, out_ptr);
                    }
                    continue;
                }
                if open_len < remainder {
                    open <<= open_len;
                    close <<= open_len;
                    remainder -= open_len;

                    open <<= 2;
                    close <<= 2;
                    remainder -= 2;

                    depth += 1;
                    continue;
                }
                break;
            }
        }
    }
}

fn lex_ident<const VEC_LEN: usize>(
    mut chunk: Chunk<VEC_LEN>,
    src_end: *const u8,
    mut out_ptr: *mut u8,
) -> (Chunk<VEC_LEN>, *mut u8) {
    let mut total_len = chunk.ident.leading_ones();

    if total_len != chunk.remainder {
        chunk.advance(total_len);
        out_ptr = write_token(out_ptr, TokenKind::Ident, total_len as u32);
        become lex_inner(chunk, src_end, out_ptr);
    }

    unsafe {
        let mut src_ptr = chunk.vec_start_ptr.add(VEC_LEN);
        loop {
            let vec = src_ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let ident = BitString::<VEC_LEN>::new(
                movemask(
                    eq(vec, b'_')
                        | in_range(vec, b'a', b'z')
                        | in_range(vec, b'A', b'Z')
                        | in_range(vec, b'0', b'9'),
                )
                .reverse_bits(),
            );
            let len = ident.leading_ones();
            total_len += len;
            if len != VEC_LEN {
                out_ptr = write_token(out_ptr, TokenKind::Ident, total_len as u32);
                chunk = Chunk::<VEC_LEN>::load(src_ptr);
                chunk.advance(len);
                become lex_inner(chunk, src_end, out_ptr);
            }
            src_ptr = src_ptr.add(VEC_LEN);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use expect_test::{Expect, expect};

    use super::*;

    const VEC_LEN: usize = 16;

    #[track_caller]
    fn check_lex(src: &str, expect: &Expect) {
        let mut input = src.as_bytes().to_vec();
        input.extend([EOF_BYTE; VEC_LEN * 2]);

        let mut buf = vec![EOF_BYTE; input.len() * 5];
        let buf = lex::<VEC_LEN>(&input, &mut buf);
        let mut buf = buf.iter().copied();

        let mut pos = 0u32;
        let mut out = String::new();
        while let Some(kind) = buf.next() {
            if kind == EOF_BYTE {
                break;
            }
            let Some(kind) = TokenKind::from_u8(kind) else {
                panic!("Invalid token kind byte: {kind} ({kind:#04x})");
            };
            let len = if kind.is_punct() {
                1
            } else {
                u32::from_ne_bytes(buf.next_chunk().expect("Expected length byte"))
            };
            let end = pos + len;
            _ = writeln!(
                out,
                "({kind:?}, {:?}, {:?})",
                pos..end,
                &src[pos as usize..end as usize]
            );

            pos = end;
        }

        expect.assert_eq(&out);
    }

    #[test]
    fn empty() { check_lex("", &expect![[""]]); }

    #[test]
    fn whitespace() {
        check_lex(" ", &expect![[r#"
            (Whitespace, 0..1, " ")
        "#]]);
        check_lex("\t", &expect![[r#"
            (Whitespace, 0..1, "\t")
        "#]]);
        check_lex("\n", &expect![[r#"
            (Whitespace, 0..1, "\n")
        "#]]);
        check_lex(" \t\n", &expect![[r#"
            (Whitespace, 0..3, " \t\n")
        "#]]);
        check_lex(" \t\n \t\n \t\n \t\n \t\n", &expect![[r#"
            (Whitespace, 0..15, " \t\n \t\n \t\n \t\n \t\n")
        "#]]);
        check_lex(" \t\n \t\n \t\n \t\n \t\n ", &expect![[r#"
            (Whitespace, 0..16, " \t\n \t\n \t\n \t\n \t\n ")
        "#]]);
        check_lex(" \t\n \t\n \t\n \t\n \t\n          ", &expect![[r#"
            (Whitespace, 0..25, " \t\n \t\n \t\n \t\n \t\n          ")
        "#]]);
    }

    #[test]
    fn line_comments() {
        check_lex("//hello\n", &expect![[r#"
            (LineComment, 0..7, "//hello")
            (Whitespace, 7..8, "\n")
        "#]]);
        check_lex("//hello\n   ", &expect![[r#"
            (LineComment, 0..7, "//hello")
            (Whitespace, 7..11, "\n   ")
        "#]]);
        check_lex("//hello", &expect![[r#"
            (LineComment, 0..7, "//hello")
        "#]]);
        check_lex("// line comment newline\n//line comment EOF", &expect![[
            r#"
            (LineComment, 0..23, "// line comment newline")
            (Whitespace, 23..24, "\n")
            (LineComment, 24..31, "//line ")
        "#
        ]]);
    }

    #[test]
    fn block_comments1() {
        check_lex("/* hello */", &expect![[r#"
        (BlockComment, 0..11, "/* hello */")
    "#]]);
    }

    #[test]
    fn block_comments2() {
        check_lex("/* hello /* nested */", &expect![[r#"
        (BlockComment, 0..21, "/* hello /* nested */")
    "#]]);
    }

    #[test]
    fn block_comments3() {
        check_lex("/* hello /* nested */ world */ goodbye", &expect![[r#"
            (BlockComment, 0..30, "/* hello /* nested */ world */")
            (Whitespace, 30..31, " ")
            (Ident, 31..38, "goodbye")
        "#]]);
    }

    #[test]
    fn block_comments4() {
        check_lex(
            "/* block comment */
            /* nested block comment /* still nested */ also still nested */
            /* /* unclosed block comment */
            oh no, still in a comment",
            &expect![[r#"
                (BlockComment, 0..177, "/* block comment */")
            "#]],
        );

        check_lex("/* EOF", &expect![[r#"
            (BlockComment, 0..6, "/* EOF")
        "#]]);
        check_lex("/*/ EOF", &expect![[r#"
            (BlockComment, 0..3, "/*/")
            (Whitespace, 3..4, " ")
            (Ident, 4..7, "EOF")
        "#]]);
        check_lex("/**/ EOF", &expect![[r#"
            (BlockComment, 0..4, "/**/")
            (Whitespace, 4..5, " ")
            (Ident, 5..8, "EOF")
        "#]]);
        check_lex("/*// EOF", &expect![[r#"
            (BlockComment, 0..3, "/*/")
            (Slash, 3..4, "/")
            (Whitespace, 4..5, " ")
            (Ident, 5..8, "EOF")
        "#]]);
        check_lex("/*/* EOF", &expect![[r#"
            (BlockComment, 0..3, "/*/")
            (Star, 3..4, "*")
            (Whitespace, 4..5, " ")
            (Ident, 5..8, "EOF")
        "#]]);
        check_lex("/*/**/ EOF", &expect![[r#"
            (BlockComment, 0..3, "/*/")
            (Star, 3..4, "*")
            (Star, 4..5, "*")
            (Slash, 5..6, "/")
            (Whitespace, 6..7, " ")
            (Ident, 7..10, "EOF")
        "#]]);
        check_lex("/*/**/ EOF", &expect![[r#"
            (BlockComment, 0..3, "/*/")
            (Star, 3..4, "*")
            (Star, 4..5, "*")
            (Slash, 5..6, "/")
            (Whitespace, 6..7, " ")
            (Ident, 7..10, "EOF")
        "#]]);
        check_lex("/* /* */ */ EOF", &expect![[r#"
            (BlockComment, 0..11, "/* /* */ */")
            (Whitespace, 11..12, " ")
            (Ident, 12..15, "EOF")
        "#]]);
        check_lex("/*/* */ */ EOF", &expect![[r#"
            (BlockComment, 0..3, "/*/")
            (Star, 3..4, "*")
            (Whitespace, 4..5, " ")
            (Star, 5..6, "*")
            (Slash, 6..7, "/")
            (Whitespace, 7..8, " ")
            (Star, 8..9, "*")
            (Slash, 9..10, "/")
            (Whitespace, 10..11, " ")
            (Ident, 11..14, "EOF")
        "#]]);
        check_lex("/*/* */*/ EOF", &expect![[""]]);
        check_lex("/*/* */*/ EOF", &expect![[""]]);
    }

    #[test]
    fn idents() {
        check_lex("a", &expect![[r#"
            (Ident, 0..1, "a")
        "#]]);
        check_lex("abc123", &expect![[r#"
            (Ident, 0..6, "abc123")
        "#]]);
        check_lex("_", &expect![[r#"
            (Ident, 0..1, "_")
        "#]]);
        check_lex("abc_123_", &expect![[r#"
            (Ident, 0..8, "abc_123_")
        "#]]);
        check_lex("abcdef123456789", &expect![[r#"
            (Ident, 0..15, "abcdef123456789")
        "#]]);
        check_lex("abcdef1234567890", &expect![[r#"
            (Ident, 0..16, "abcdef1234567890")
        "#]]);
        check_lex("abcdef1234567890xyz", &expect![[r#"
            (Ident, 0..19, "abcdef1234567890xyz")
        "#]]);
    }

    #[test]
    fn idents_and_whitespace() {
        check_lex("a b c", &expect![[r#"
            (Ident, 0..1, "a")
            (Whitespace, 1..2, " ")
            (Ident, 2..3, "b")
            (Whitespace, 3..4, " ")
            (Ident, 4..5, "c")
        "#]]);

        check_lex("abc  def  ghi", &expect![[r#"
            (Ident, 0..3, "abc")
            (Whitespace, 3..5, "  ")
            (Ident, 5..8, "def")
            (Whitespace, 8..10, "  ")
            (Ident, 10..13, "ghi")
        "#]]);
    }

    #[test]
    fn punctuation() {
        check_lex("!#$%&()*+,-./:;<=>?[]^{|}~", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (Dollar, 2..3, "$")
            (Percent, 3..4, "%")
            (Ampersand, 4..5, "&")
            (LParen, 5..6, "(")
            (RParen, 6..7, ")")
            (Star, 7..8, "*")
            (Plus, 8..9, "+")
            (Comma, 9..10, ",")
            (Minus, 10..11, "-")
            (Dot, 11..12, ".")
            (Slash, 12..13, "/")
            (Colon, 13..14, ":")
            (Semicolon, 14..15, ";")
            (Lt, 15..16, "<")
            (Eq, 16..17, "=")
            (Gt, 17..18, ">")
            (Question, 18..19, "?")
            (LSquare, 19..20, "[")
            (RSquare, 20..21, "]")
            (Caret, 21..22, "^")
            (LCurly, 22..23, "{")
            (Bar, 23..24, "|")
            (RCurly, 24..25, "}")
            (Tilde, 25..26, "~")
        "##]]);
        check_lex("!#///*\n", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (LineComment, 2..6, "///*")
            (Whitespace, 6..7, "\n")
        "##]]);
    }

    #[test]
    fn numbers() {
        check_lex(
            "0 1234567890 123_456 123suffix 1.2 0.1 0. 0..1 0. 1e 1E 1e+ 1e- 1e+2 1e+2suffix",
            &expect![[""]],
        );
        check_lex(
            "0b10_1010asdfbz 0o755as_dfzxc 0xDEADBE_EFasdfzxc",
            &expect![[""]],
        );
    }

    #[test]
    fn chars() {
        check_lex("'a' '\n' '\\'' '' 'foo'", &expect![[""]]);
        check_lex("b'a' b'\n' b'\\'' b'' b'foo'", &expect![[""]]);
    }

    #[test]
    fn strings() {
        check_lex(r#""" "simple" "escaped \" quote" "unterminated"#, &expect![
            [""]
        ]);
        check_lex(
            r#"b"" b"simple" b"escaped \" quote" b"unterminated"#,
            &expect![[""]],
        );
        check_lex(
            r#"c"" c"simple" c"escaped \" quote" c"unterminated"#,
            &expect![[""]],
        );
    }

    #[test]
    fn raw_strings() {
        check_lex(
            r#"
    r"raw string\"
    br"raw string\"
    cr"raw string\"
    r"unterminated
    "#,
            &expect![[""]],
        );
    }

    #[test]
    fn hash_strings() {
        check_lex(
            r###"
    r#""#
    r##""##
    r#"raw string""""""""#
    r#"""""""""#
    r##" ##"" "##
    r#"unterminated"
    "###,
            &expect![[""]],
        );

        check_lex(r#"r#""#, &expect![[""]]);
        check_lex(r#"r#"""#, &expect![[""]]);
        check_lex(
            r###"
    br#""#
    br##""##
    br#"raw string""""""""#
    br#"""""""""#
    br##" ##"" "##
    br#"unterminated"
    "###,
            &expect![[""]],
        );

        check_lex(
            r###"
    cr#""#
    cr##""##
    cr#"raw string""""""""#
    cr#"""""""""#
    cr##" ##"" "##
    cr#"unterminated"
    "###,
            &expect![[""]],
        );
    }
}
