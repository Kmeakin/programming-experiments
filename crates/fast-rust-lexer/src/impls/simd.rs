use core::slice;
use std::fmt;
use std::marker::PhantomData;
use std::simd::prelude::*;

use crate::TokenKind;
use crate::utils::bitstring::BitString;
use crate::utils::simdx::{eq, in_range, movemask};
use crate::utils::write_and_advance;

pub const EOF_BYTE: u8 = 0xFF;

unsafe fn write_token(out: *mut u8, kind: TokenKind, len: u32) -> *mut u8 {
    unsafe {
        let out = write_and_advance(out, kind as u8);
        write_and_advance(out.cast(), len).cast()
    }
}

#[derive(Copy, Clone)]
struct Chunk<const VEC_LEN: usize> {
    remainder:   usize,
    whitespace:  BitString<VEC_LEN>,
    newline:     BitString<VEC_LEN>,
    slash_slash: BitString<VEC_LEN>,
    slash_star:  BitString<VEC_LEN>,
    star_slash:  BitString<VEC_LEN>,
    digits:      BitString<VEC_LEN>,
    ident:       BitString<VEC_LEN>,
    quote:       BitString<VEC_LEN>,
    apostrophe:  BitString<VEC_LEN>,
    punctuation: BitString<VEC_LEN>,
    hash:        BitString<VEC_LEN>,
}

impl<const VEC_LEN: usize> Chunk<VEC_LEN> {
    fn new(bytes: [u8; VEC_LEN], byte1: u8) -> Self {
        let bytes = Simd::from_array(bytes);

        let newline = movemask(eq(bytes, b'\n')).reverse_bits();
        let whitespace = movemask(eq(bytes, b' ') | eq(bytes, b'\t')).reverse_bits() | newline;

        let slash = movemask(eq(bytes, b'/')).reverse_bits();
        let star = movemask(eq(bytes, b'*')).reverse_bits();

        let slash1 = slash << 1 | (u64::from(byte1 == b'/') << (64 - VEC_LEN));
        let star1 = star << 1 | (u64::from(byte1 == b'*') << (64 - VEC_LEN));

        let slash_slash = slash & slash1;
        let slash_star = slash & star1;
        let star_slash = star & slash1;

        let digits = movemask(in_range(bytes, b'0', b'9')).reverse_bits();
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
            remainder:   VEC_LEN,
            whitespace:  BitString::new(whitespace),
            newline:     BitString::new(newline),
            slash_slash: BitString::new(slash_slash),
            slash_star:  BitString::new(slash_star),
            star_slash:  BitString::new(star_slash),
            digits:      BitString::new(digits),
            ident:       BitString::new(ident),
            quote:       BitString::new(quote),
            apostrophe:  BitString::new(apostrophe),
            punctuation: BitString::new(punctuation),
            hash:        BitString::new(movemask(eq(bytes, b'#')).reverse_bits()),
        }
    }

    fn is_empty(&self) -> bool { self.remainder == 0 }

    fn advance(&mut self, amount: usize) {
        debug_assert!(amount <= VEC_LEN, "amount = {amount}, VEC_LEN = {VEC_LEN}");
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
        self.digits <<= amount;
        self.ident <<= amount;
        self.quote <<= amount;
        self.apostrophe <<= amount;
        self.punctuation <<= amount;
        self.hash <<= amount;
    }

    /// Eat leading whitespace in this chunk, returning `true` if we reached
    /// the end of the chunk.
    fn eat_leading_whitespace(&mut self) -> bool {
        let len = self.whitespace.leading_ones();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }

    /// Eat leading punctuation in this chunk, returning `true` if we reached
    /// the end of the chunk.
    fn eat_leading_punctuation(&mut self) -> bool {
        let len = self.punctuation.leading_ones();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }

    /// Eat upto (but not including) a newline, returning `true` if we reached
    /// the end of the chunk.
    fn eat_upto_newline(&mut self) -> bool {
        let len = self.newline.leading_zeros();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }

    /// Eat leading ident chars in this chunk, returning `true` if we reached
    /// the end of the chunk.
    fn eat_leading_ident(&mut self) -> bool {
        let len = self.ident.leading_ones();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }

    /// Eat leading digit chars in this chunk, returning `true` if we reached
    /// the end of the chunk.
    fn eat_leading_digits(&mut self) -> bool {
        let len = self.digits.leading_ones();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }

    /// Eat upto the next '/*' or '*/'. Returns `None` if we reached the end of
    /// the chunk.
    fn next_block_comment(&mut self) -> Option<OpenOrClose> {
        let open = self.slash_star.leading_zeros();
        let close = self.star_slash.leading_zeros();

        if open < close {
            self.advance((open + 2).min(self.remainder));
            return Some(OpenOrClose::Open);
        }

        if close < self.remainder {
            self.advance((close + 2).min(self.remainder));
            return Some(OpenOrClose::Close);
        }

        debug_assert!(open >= self.remainder);
        debug_assert!(close >= self.remainder);
        None
    }

    /// Eat upto (but not including) an apostrophe, returning `true` if we
    /// reached the end of the chunk.
    fn eat_upto_apostrophe(&mut self) -> bool {
        let len = self.apostrophe.leading_zeros();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }

    /// Eat upto (but not including) a quote, returning `true` if we
    /// reached the end of the chunk.
    fn eat_upto_quote(&mut self) -> bool {
        let len = self.quote.leading_zeros();
        if len >= self.remainder {
            return true;
        }
        self.advance(len);
        false
    }
}

enum OpenOrClose {
    Open,
    Close,
}

impl<const VEC_LEN: usize> fmt::Debug for Chunk<VEC_LEN> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chunk")
            .field("remainder\t", &self.remainder)
            .field("whitespace\t", &self.whitespace)
            .field("newline\t", &self.newline)
            .field("slash_slash\t", &self.slash_slash)
            .field("slash_star\t", &self.slash_star)
            .field("star_slash\t", &self.star_slash)
            .field("digits\t", &self.digits)
            .field("ident\t", &self.ident)
            .field("quote\t", &self.quote)
            .field("apostrophe\t", &self.apostrophe)
            .field("punctuation\t", &self.punctuation)
            .field("hash\t", &self.hash)
            .finish()
    }
}

struct Cursor<'src, const VEC_LEN: usize> {
    chunk_start: *const u8,
    src_start:   *const u8,
    src_end:     *const u8,
    chunk:       Chunk<VEC_LEN>,
    lifetime:    PhantomData<&'src [u8]>,
}

impl<const VEC_LEN: usize> Cursor<'_, VEC_LEN> {
    unsafe fn new(src: &[u8]) -> Self {
        debug_assert!(src.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
        unsafe {
            let std::ops::Range { start, end } = src.as_ptr_range();
            let chunk_start = start;
            let bytes = chunk_start.cast::<[u8; VEC_LEN]>().read();
            let byte1 = chunk_start.add(VEC_LEN).read();

            Self {
                chunk_start: (chunk_start),
                src_start:   start,
                src_end:     end.sub(VEC_LEN * 2),
                chunk:       Chunk::new(bytes, byte1),
                lifetime:    PhantomData,
            }
        }
    }

    fn next_chunk(&mut self) -> Option<Chunk<VEC_LEN>> {
        unsafe {
            let chunk_start = self.chunk_start.add(VEC_LEN);
            if chunk_start >= self.src_end {
                return None;
            }
            let bytes = chunk_start.cast::<[u8; VEC_LEN]>().read();
            let byte1 = chunk_start.add(VEC_LEN).read();
            self.chunk_start = chunk_start;
            Some(Chunk::new(bytes, byte1))
        }
    }

    fn next_chunk_unchecked(&mut self) -> Chunk<VEC_LEN> {
        unsafe {
            let chunk_start = self.chunk_start.add(VEC_LEN);
            let bytes = chunk_start.cast::<[u8; VEC_LEN]>().read();
            let byte1 = chunk_start.add(VEC_LEN).read();
            self.chunk_start = chunk_start;
            Chunk::new(bytes, byte1)
        }
    }

    fn ptr(&self) -> *const u8 {
        unsafe { self.chunk_start.add(VEC_LEN).sub(self.chunk.remainder) }
    }

    fn peek(&self) -> u8 { unsafe { self.ptr().read() } }

    fn token_len(&self, token_start: *const u8) -> u32 {
        let token_end = self.ptr();
        unsafe { token_end.offset_from_unsigned(token_start) as u32 }
    }

    fn token_len_eof(&self, token_start: *const u8) -> u32 {
        let token_end = self.src_end;
        unsafe { token_end.offset_from_unsigned(token_start) as u32 }
    }

    fn refill_if_needed(&mut self) {
        if self.chunk.is_empty()
            && let Some(chunk) = self.next_chunk()
        {
            self.chunk = chunk;
        }
    }
}

pub fn lex<'out, const VEC_LEN: usize>(src: &[u8], out: &'out mut [u8]) -> &'out mut [u8] {
    debug_assert!(out.len() >= src.len() * 5); // Each token is at most 5 bytes (kind + len)
    let mut cursor = unsafe { Cursor::<VEC_LEN>::new(src) };
    let out_start = out.as_mut_ptr();
    let out_end = lex_loop(&mut cursor, out_start);
    let out_len = unsafe { out_end.offset_from_unsigned(out_start) };
    &mut out[..out_len]
}

fn lex_loop<const VEC_LEN: usize>(cursor: &mut Cursor<'_, VEC_LEN>, mut out: *mut u8) -> *mut u8 {
    unsafe {
        'outer: loop {
            let mut token_start = cursor.ptr();
            if cursor.chunk.whitespace.any() {
                while cursor.chunk.eat_leading_whitespace() {
                    cursor.chunk = cursor.next_chunk_unchecked();
                }
                let token_len = cursor.token_len(token_start);
                out = write_token(out, TokenKind::Whitespace, token_len);
                continue 'outer;
            }

            if cursor.chunk.punctuation.any() {
                while cursor.chunk.eat_leading_punctuation() {
                    cursor.chunk = cursor.next_chunk_unchecked();
                }
                let token_len = cursor.token_len(token_start);
                token_start.copy_to_nonoverlapping(out, token_len as usize);
                out = out.add(token_len as usize);
                continue 'outer;
            }

            if cursor.chunk.digits.any() {
                while cursor.chunk.eat_leading_digits() {
                    cursor.chunk = cursor.next_chunk_unchecked();
                }
                let mut kind = match (cursor.ptr().read(), cursor.ptr().add(1).read()) {
                    (b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_') => TokenKind::Int,
                    (b'.', _) => {
                        cursor.chunk.advance(1);
                        cursor.refill_if_needed();

                        // TODO: underscores too
                        while cursor.chunk.eat_leading_digits() {
                            cursor.chunk = cursor.next_chunk_unchecked();
                        }
                        TokenKind::Float
                    }
                    _ => TokenKind::Int,
                };

                if let b'e' | b'E' = cursor.ptr().read() {
                    kind = TokenKind::Float;
                    cursor.chunk.advance(1);
                    cursor.refill_if_needed();

                    if let b'+' | b'-' = cursor.ptr().read() {
                        cursor.chunk.advance(1);
                        cursor.refill_if_needed();
                    }
                }

                while cursor.chunk.eat_leading_ident() {
                    cursor.chunk = cursor.next_chunk_unchecked();
                }
                let token_len = cursor.token_len(token_start);
                out = write_token(out, kind, token_len);
                continue 'outer;
            }

            if cursor.chunk.ident.any() {
                while cursor.chunk.eat_leading_ident() {
                    cursor.chunk = cursor.next_chunk_unchecked();
                }
                let token_len = cursor.token_len(token_start);
                out = write_token(out, TokenKind::Ident, token_len);
                continue 'outer;
            }

            if cursor.chunk.slash_slash.any() {
                while cursor.chunk.eat_upto_newline() {
                    let Some(next_chunk) = cursor.next_chunk() else {
                        let token_len = cursor.token_len_eof(token_start);
                        let out = write_token(out, TokenKind::LineComment, token_len);
                        return out;
                    };
                    cursor.chunk = next_chunk;
                }
                let token_len = cursor.token_len(token_start);
                out = write_token(out, TokenKind::LineComment, token_len);
                continue 'outer;
            }

            if cursor.chunk.slash_star.any() {
                let mut depth = 0u32;
                loop {
                    match cursor.chunk.next_block_comment() {
                        Some(OpenOrClose::Open) => depth += 1,
                        Some(OpenOrClose::Close) => {
                            depth -= 1;
                            if depth == 0 {
                                let token_len = cursor.token_len(token_start);
                                out = write_token(out, TokenKind::BlockComment, token_len);
                                continue 'outer;
                            }
                        }
                        None => match cursor.next_chunk() {
                            Some(chunk) => cursor.chunk = chunk,
                            None => {
                                let token_len = cursor.token_len_eof(token_start);
                                out = write_token(out, TokenKind::BlockComment, token_len);
                                return out;
                            }
                        },
                    }
                }
            }

            if cursor.chunk.quote.any() {
                let mut kind = TokenKind::Str;
                if token_start > cursor.src_start {
                    match token_start.sub(1).read() {
                        b'r' => {
                            let kind;
                            (out, token_start, kind) = match token_start.sub(1) == cursor.src_start
                            {
                                true => (out.sub(5), token_start.sub(1), TokenKind::RawStr),
                                false => match token_start.sub(2).read() {
                                    b'b' => (out.sub(5), token_start.sub(2), TokenKind::RawByteStr),
                                    b'c' => (out.sub(5), token_start.sub(2), TokenKind::RawCStr),
                                    _ => (out.sub(5), token_start.sub(1), TokenKind::RawStr),
                                },
                            };
                            cursor.chunk.advance(1);
                            cursor.refill_if_needed();
                            while cursor.chunk.eat_upto_quote() {
                                match cursor.next_chunk() {
                                    Some(next_chunk) => cursor.chunk = next_chunk,
                                    None => {
                                        let token_len = cursor.token_len_eof(token_start);
                                        return write_token(out, kind, token_len);
                                    }
                                }
                            }

                            cursor.chunk.advance(1);
                            let token_len = cursor.token_len(token_start);
                            out = write_token(out, kind, token_len);
                            continue 'outer;
                        }
                        b'#' => {
                            let mut num_hashes = 1;
                            let mut p = token_start.sub(2);
                            loop {
                                if p.read() != b'#' {
                                    break;
                                }
                                num_hashes += 1;
                                if p <= cursor.src_start {
                                    break;
                                }
                                p = p.sub(1);
                            }

                            let lexeme = slice::from_ptr_range(cursor.src_start..p.add(1));
                            let kind;
                            (out, token_start, kind) = if lexeme.ends_with(b"br") {
                                (out.sub(5).sub(num_hashes), p.sub(1), TokenKind::RawByteStr)
                            } else if lexeme.ends_with(b"cr") {
                                (out.sub(5).sub(num_hashes), p.sub(1), TokenKind::RawCStr)
                            } else if lexeme.ends_with(b"r") {
                                (out.sub(5).sub(num_hashes), p.sub(0), TokenKind::RawStr)
                            } else {
                                (out.sub(num_hashes), p.sub(0), TokenKind::GuardedStr)
                            };

                            cursor.chunk.advance(1);
                            cursor.refill_if_needed();
                            loop {
                                while cursor.chunk.eat_upto_quote() {
                                    match cursor.next_chunk() {
                                        Some(next_chunk) => cursor.chunk = next_chunk,
                                        None => {
                                            let token_len = cursor.token_len_eof(token_start);
                                            return write_token(out, kind, token_len);
                                        }
                                    }
                                }

                                debug_assert_eq!(cursor.ptr().read(), b'"');
                                cursor.chunk.advance(1);
                                cursor.refill_if_needed();

                                let mut num_remaining_hashes = num_hashes;
                                loop {
                                    let len = cursor.chunk.hash.leading_ones();
                                    if len >= num_remaining_hashes {
                                        cursor.chunk.advance(num_remaining_hashes);
                                        let token_len = cursor.token_len(token_start);
                                        out = write_token(out, kind, token_len);
                                        continue 'outer;
                                    }

                                    num_remaining_hashes -= len;
                                    if len >= cursor.chunk.remainder {
                                        cursor.chunk = cursor.next_chunk_unchecked();
                                    } else {
                                        cursor.chunk.advance(len);
                                        break;
                                    }
                                }
                            }
                        }
                        b'b' => {
                            out = out.sub(5);
                            token_start = token_start.sub(1);
                            kind = TokenKind::ByteStr;
                        }
                        b'c' => {
                            out = out.sub(5);
                            token_start = token_start.sub(1);
                            kind = TokenKind::CStr;
                        }
                        _ => {}
                    }
                }

                cursor.chunk.advance(1);
                cursor.refill_if_needed();
                loop {
                    while cursor.chunk.eat_upto_quote() {
                        match cursor.next_chunk() {
                            Some(next_chunk) => cursor.chunk = next_chunk,
                            None => {
                                let token_len = cursor.token_len_eof(token_start);
                                let out = write_token(out, kind, token_len);
                                return out;
                            }
                        }
                    }

                    let mut num_backslashes = 0;
                    let mut p = cursor.ptr();
                    while p > token_start {
                        p = p.sub(1);
                        if p.read() != b'\\' {
                            break;
                        }
                        num_backslashes += 1;
                    }

                    cursor.chunk.advance(1);
                    if num_backslashes % 2 == 0 {
                        let token_len = cursor.token_len(token_start);
                        out = write_token(out, kind, token_len);
                        continue 'outer;
                    }
                }
            }

            if cursor.chunk.apostrophe.any() {
                let mut kind = TokenKind::Char;
                if token_start > cursor.src_start && token_start.sub(1).read() == b'b' {
                    kind = TokenKind::Byte;
                    token_start = token_start.sub(1);
                    out = out.sub(5);
                }

                cursor.chunk.advance(1);
                cursor.refill_if_needed();
                if cursor.chunk.ident.any() {
                    while cursor.chunk.eat_leading_ident() {
                        cursor.chunk = cursor.next_chunk_unchecked();
                    }
                    cursor.refill_if_needed();
                    let kind = if cursor.chunk.apostrophe.any() {
                        cursor.chunk.advance(1);
                        kind
                    } else {
                        TokenKind::Lifetime
                    };
                    let len = cursor.token_len(token_start);
                    out = write_token(out, kind, len);
                    continue 'outer;
                }
                loop {
                    while cursor.chunk.eat_upto_apostrophe() {
                        let Some(next_chunk) = cursor.next_chunk() else {
                            let token_len = cursor.token_len_eof(token_start);
                            let out = write_token(out, kind, token_len);
                            return out;
                        };
                        cursor.chunk = next_chunk;
                    }

                    let mut end = cursor.ptr();
                    let mut num_backslashes = 0;
                    while end > token_start {
                        end = end.sub(1);
                        if end.read() != b'\\' {
                            break;
                        }
                        num_backslashes += 1;
                    }

                    cursor.chunk.advance(1);
                    if num_backslashes % 2 == 0 {
                        let token_len = cursor.token_len(token_start);
                        out = write_token(out, kind, token_len);
                        continue 'outer;
                    }
                }
            }

            if cursor.peek() == EOF_BYTE {
                return out;
            }

            cursor.chunk = cursor.next_chunk().unwrap();
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
    fn check(src: &str, expect: &Expect) {
        let mut input = src.as_bytes().to_vec();
        input.extend([EOF_BYTE; VEC_LEN * 2]);

        let mut buf = vec![EOF_BYTE; input.len() * 10];
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
            let len = match kind.is_punct() {
                true => 1,
                false => u32::from_ne_bytes(buf.next_chunk().expect("Expected length byte")),
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
    fn empty() { check("", &expect![[""]]); }

    #[test]
    fn whitespace() {
        check(" ", &expect![[r#"
            (Whitespace, 0..1, " ")
        "#]]);
        check("\t", &expect![[r#"
            (Whitespace, 0..1, "\t")
        "#]]);
        check("\n", &expect![[r#"
            (Whitespace, 0..1, "\n")
        "#]]);
        check(" \t\n", &expect![[r#"
            (Whitespace, 0..3, " \t\n")
        "#]]);
        check(" \t\n \t\n \t\n \t\n \t\n", &expect![[r#"
            (Whitespace, 0..15, " \t\n \t\n \t\n \t\n \t\n")
        "#]]);
        check(" \t\n \t\n \t\n \t\n \t\n ", &expect![[r#"
            (Whitespace, 0..16, " \t\n \t\n \t\n \t\n \t\n ")
        "#]]);
        check(" \t\n \t\n \t\n \t\n \t\n          ", &expect![[r#"
            (Whitespace, 0..25, " \t\n \t\n \t\n \t\n \t\n          ")
        "#]]);
    }

    #[test]
    fn line_comments() {
        check("//hello\n", &expect![[r#"
            (LineComment, 0..7, "//hello")
            (Whitespace, 7..8, "\n")
        "#]]);
        check("//hello\n   ", &expect![[r#"
            (LineComment, 0..7, "//hello")
            (Whitespace, 7..11, "\n   ")
        "#]]);
        check("//hello", &expect![[r#"
            (LineComment, 0..7, "//hello")
        "#]]);
        check("// line comment newline\n//line comment EOF", &expect![[
            r#"
            (LineComment, 0..23, "// line comment newline")
            (Whitespace, 23..24, "\n")
            (LineComment, 24..42, "//line comment EOF")
        "#
        ]]);
    }

    #[test]
    fn block_comments1() {
        check("/* hello */", &expect![[r#"
        (BlockComment, 0..11, "/* hello */")
    "#]]);
    }

    #[test]
    fn block_comments2() {
        check("/* hello /* nested */", &expect![[r#"
        (BlockComment, 0..21, "/* hello /* nested */")
    "#]]);
    }

    #[test]
    fn block_comments3() {
        check("/* hello /* nested */ world */ goodbye", &expect![[r#"
            (BlockComment, 0..30, "/* hello /* nested */ world */")
            (Whitespace, 30..31, " ")
            (Ident, 31..38, "goodbye")
        "#]]);
    }

    #[test]
    fn block_comments4() {
        check(
            "_23456789abc/* /* unclosed block comment */
            oh no, still in a comment",
            &expect![[r#"
            (Ident, 0..12, "_23456789abc")
            (BlockComment, 12..81, "/* /* unclosed block comment */\n            oh no, still in a comment")
            "#]],
        );
    }

    #[test]
    fn block_comments5() {
        check(
            "/* block comment */
            /* nested block comment /* still nested */ also still nested */
            /* /* unclosed block comment */
            oh no, still in a comment",
            &expect![[r#"
            (BlockComment, 0..19, "/* block comment */")
            (Whitespace, 19..32, "\n            ")
            (BlockComment, 32..95, "/* nested block comment /* still nested */ also still nested */")
            (Whitespace, 95..108, "\n            ")
            (BlockComment, 108..177, "/* /* unclosed block comment */\n            oh no, still in a comment")
        "#]],
        );
    }

    #[test]
    fn block_comments6() {
        check("/* EOF", &expect![[r#"
            (BlockComment, 0..6, "/* EOF")
        "#]]);
        check("/*/ EOF", &expect![[r#"
            (BlockComment, 0..7, "/*/ EOF")
        "#]]);
        check("/**/ EOF", &expect![[r#"
            (BlockComment, 0..4, "/**/")
            (Whitespace, 4..5, " ")
            (Ident, 5..8, "EOF")
        "#]]);
        check("/*// EOF", &expect![[r#"
            (BlockComment, 0..8, "/*// EOF")
        "#]]);
        check("/*/* EOF", &expect![[r#"
            (BlockComment, 0..8, "/*/* EOF")
        "#]]);
        check("/*/**/ EOF", &expect![[r#"
            (BlockComment, 0..10, "/*/**/ EOF")
        "#]]);
        check("/* /* */ */ EOF", &expect![[r#"
            (BlockComment, 0..11, "/* /* */ */")
            (Whitespace, 11..12, " ")
            (Ident, 12..15, "EOF")
        "#]]);
        check("/*/* */ */ EOF", &expect![[r#"
            (BlockComment, 0..10, "/*/* */ */")
            (Whitespace, 10..11, " ")
            (Ident, 11..14, "EOF")
        "#]]);
        check("/*/* */*/ EOF", &expect![[r#"
            (BlockComment, 0..9, "/*/* */*/")
            (Whitespace, 9..10, " ")
            (Ident, 10..13, "EOF")
        "#]]);
        check("/*/**/*/ EOF", &expect![[r#"
            (BlockComment, 0..8, "/*/**/*/")
            (Whitespace, 8..9, " ")
            (Ident, 9..12, "EOF")
        "#]]);
        check("/**//**/ EOF", &expect![[r#"
            (BlockComment, 0..4, "/**/")
            (BlockComment, 4..8, "/**/")
            (Whitespace, 8..9, " ")
            (Ident, 9..12, "EOF")
        "#]]);
    }

    #[test]
    fn idents() {
        check("a", &expect![[r#"
            (Ident, 0..1, "a")
        "#]]);
        check("abc123", &expect![[r#"
            (Ident, 0..6, "abc123")
        "#]]);
        check("_", &expect![[r#"
            (Ident, 0..1, "_")
        "#]]);
        check("abc_123_", &expect![[r#"
            (Ident, 0..8, "abc_123_")
        "#]]);
        check("abcdef123456789", &expect![[r#"
            (Ident, 0..15, "abcdef123456789")
        "#]]);
        check("abcdef1234567890", &expect![[r#"
            (Ident, 0..16, "abcdef1234567890")
        "#]]);
        check("abcdef1234567890xyz", &expect![[r#"
            (Ident, 0..19, "abcdef1234567890xyz")
        "#]]);
    }

    #[test]
    fn idents_and_whitespace() {
        check("a b c", &expect![[r#"
            (Ident, 0..1, "a")
            (Whitespace, 1..2, " ")
            (Ident, 2..3, "b")
            (Whitespace, 3..4, " ")
            (Ident, 4..5, "c")
        "#]]);

        check("abc  def  ghi", &expect![[r#"
            (Ident, 0..3, "abc")
            (Whitespace, 3..5, "  ")
            (Ident, 5..8, "def")
            (Whitespace, 8..10, "  ")
            (Ident, 10..13, "ghi")
        "#]]);
    }

    #[test]
    fn punctuation() {
        check("!#$%&()*+,-./:;<=>?[]^{|}~", &expect![[r##"
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
        check("!#///*\n", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (LineComment, 2..6, "///*")
            (Whitespace, 6..7, "\n")
        "##]]);
        check("!#/*\n*/~>", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (BlockComment, 2..7, "/*\n*/")
            (Tilde, 7..8, "~")
            (Gt, 8..9, ">")
        "##]]);
    }

    #[test]
    fn numbers() {
        check(
            "0 1234567890 123_456 123suffix 1.2 0.1 0. 0..1 0. 1e 1E 1e+ 1e- 1e+2 1e+2suffix",
            &expect![[r#"
                (Int, 0..1, "0")
                (Whitespace, 1..2, " ")
                (Int, 2..12, "1234567890")
                (Whitespace, 12..13, " ")
                (Int, 13..20, "123_456")
                (Whitespace, 20..21, " ")
                (Int, 21..30, "123suffix")
                (Whitespace, 30..31, " ")
                (Float, 31..34, "1.2")
                (Whitespace, 34..35, " ")
                (Float, 35..38, "0.1")
                (Whitespace, 38..39, " ")
                (Float, 39..41, "0.")
                (Whitespace, 41..42, " ")
                (Int, 42..43, "0")
                (Dot, 43..44, ".")
                (Dot, 44..45, ".")
                (Int, 45..46, "1")
                (Whitespace, 46..47, " ")
                (Float, 47..49, "0.")
                (Whitespace, 49..50, " ")
                (Float, 50..52, "1e")
                (Whitespace, 52..53, " ")
                (Float, 53..55, "1E")
                (Whitespace, 55..56, " ")
                (Float, 56..59, "1e+")
                (Whitespace, 59..60, " ")
                (Float, 60..63, "1e-")
                (Whitespace, 63..64, " ")
                (Float, 64..68, "1e+2")
                (Whitespace, 68..69, " ")
                (Float, 69..79, "1e+2suffix")
            "#]],
        );

        check(
            "0b10_1010asdfbz 0o755as_dfzxc 0xDEADBE_EFasdfzxc",
            &expect![[r#"
                (Int, 0..15, "0b10_1010asdfbz")
                (Whitespace, 15..16, " ")
                (Int, 16..29, "0o755as_dfzxc")
                (Whitespace, 29..30, " ")
                (Int, 30..48, "0xDEADBE_EFasdfzxc")
            "#]],
        );
    }

    #[test]
    fn chars() {
        check("'a' '\n' '\\'' '' 'foo'", &expect![[r#"
            (Char, 0..3, "'a'")
            (Whitespace, 3..4, " ")
            (Char, 4..7, "'\n'")
            (Whitespace, 7..8, " ")
            (Char, 8..12, "'\\''")
            (Whitespace, 12..13, " ")
            (Char, 13..15, "''")
            (Whitespace, 15..16, " ")
            (Char, 16..21, "'foo'")
        "#]]);
        check("b'a' b'\n' b'\\'' b'' b'foo'", &expect![[r#"
            (Byte, 0..4, "b'a'")
            (Whitespace, 4..5, " ")
            (Byte, 5..9, "b'\n'")
            (Whitespace, 9..10, " ")
            (Byte, 10..15, "b'\\''")
            (Whitespace, 15..16, " ")
            (Byte, 16..19, "b''")
            (Whitespace, 19..20, " ")
            (Byte, 20..26, "b'foo'")
        "#]]);
    }

    #[test]
    fn strings() {
        check(r#""" "simple" "escaped \" quote" "unterminated"#, &expect![
            [r#"
                (Str, 0..2, "\"\"")
                (Whitespace, 2..3, " ")
                (Str, 3..11, "\"simple\"")
                (Whitespace, 11..12, " ")
                (Str, 12..30, "\"escaped \\\" quote\"")
                (Whitespace, 30..31, " ")
                (Str, 31..44, "\"unterminated")
            "#]
        ]);
        check(
            r#"b"" b"simple" b"escaped \" quote" b"unterminated"#,
            &expect![[r#"
                (ByteStr, 0..3, "b\"\"")
                (Whitespace, 3..4, " ")
                (ByteStr, 4..13, "b\"simple\"")
                (Whitespace, 13..14, " ")
                (ByteStr, 14..33, "b\"escaped \\\" quote\"")
                (Whitespace, 33..34, " ")
                (ByteStr, 34..48, "b\"unterminated")
            "#]],
        );
        check(
            r#"c"" c"simple" c"escaped \" quote" c"unterminated"#,
            &expect![[r#"
                (CStr, 0..3, "c\"\"")
                (Whitespace, 3..4, " ")
                (CStr, 4..13, "c\"simple\"")
                (Whitespace, 13..14, " ")
                (CStr, 14..33, "c\"escaped \\\" quote\"")
                (Whitespace, 33..34, " ")
                (CStr, 34..48, "c\"unterminated")
            "#]],
        );
    }

    #[test]
    fn raw_strings() {
        check(
            r#"
    r"raw string\"
    br"raw string\"
    cr"raw string\"
    r"unterminated
    "#,
            &expect![[r#"
                (Whitespace, 0..5, "\n    ")
                (RawStr, 5..19, "r\"raw string\\\"")
                (Whitespace, 19..24, "\n    ")
                (RawByteStr, 24..39, "br\"raw string\\\"")
                (Whitespace, 39..44, "\n    ")
                (RawCStr, 44..59, "cr\"raw string\\\"")
                (Whitespace, 59..64, "\n    ")
                (RawStr, 64..83, "r\"unterminated\n    ")
            "#]],
        );
    }

    #[test]
    fn hash_strings() {
        check(
            r###"r#""# r##""## r#"raw string""""""""# r#"""""""""# r##" ##"" "## r#"unterminated" "###,
            &expect![[r###"
                (RawStr, 0..5, "r#\"\"#")
                (Whitespace, 5..6, " ")
                (RawStr, 6..13, "r##\"\"##")
                (Whitespace, 13..14, " ")
                (RawStr, 14..36, "r#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 36..37, " ")
                (RawStr, 37..49, "r#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 49..50, " ")
                (RawStr, 50..63, "r##\" ##\"\" \"##")
                (Whitespace, 63..64, " ")
                (RawStr, 64..81, "r#\"unterminated\" ")
            "###]],
        );

        check(r#"r#""#, &expect![[r#"
            (RawStr, 0..3, "r#\"")
        "#]]);
        check(r#"r#"""#, &expect![[r#"
            (RawStr, 0..4, "r#\"\"")
        "#]]);
        check(
            r###"
    br#""#
    br##""##
    br#"raw string""""""""#
    br#"""""""""#
    br##" ##"" "##
    br#"unterminated"
    "###,
            &expect![[r###"
                (Whitespace, 0..5, "\n    ")
                (RawByteStr, 5..11, "br#\"\"#")
                (Whitespace, 11..16, "\n    ")
                (RawByteStr, 16..24, "br##\"\"##")
                (Whitespace, 24..29, "\n    ")
                (RawByteStr, 29..52, "br#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 52..57, "\n    ")
                (RawByteStr, 57..70, "br#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 70..75, "\n    ")
                (RawByteStr, 75..89, "br##\" ##\"\" \"##")
                (Whitespace, 89..94, "\n    ")
                (RawByteStr, 94..116, "br#\"unterminated\"\n    ")
            "###]],
        );

        check(
            r###"
    cr#""#
    cr##""##
    cr#"raw string""""""""#
    cr#"""""""""#
    cr##" ##"" "##
    cr#"unterminated"
    "###,
            &expect![[r###"
                (Whitespace, 0..5, "\n    ")
                (RawCStr, 5..11, "cr#\"\"#")
                (Whitespace, 11..16, "\n    ")
                (RawCStr, 16..24, "cr##\"\"##")
                (Whitespace, 24..29, "\n    ")
                (RawCStr, 29..52, "cr#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 52..57, "\n    ")
                (RawCStr, 57..70, "cr#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 70..75, "\n    ")
                (RawCStr, 75..89, "cr##\" ##\"\" \"##")
                (Whitespace, 89..94, "\n    ")
                (RawCStr, 94..116, "cr#\"unterminated\"\n    ")
            "###]],
        );
    }
}
