use std::simd::prelude::*;

use crate::TokenKind;
use crate::utils::bitstring::BitString;
use crate::utils::simdx::*;
use crate::utils::{align_down, write_and_advance};

pub const EOF_BYTE: u8 = 0xFF;

pub fn lex_16<'out>(src: &[u8], out: &'out mut [u8]) -> &'out mut [u8] { lex::<16>(src, out) }
pub fn lex_32<'out>(src: &[u8], out: &'out mut [u8]) -> &'out mut [u8] { lex::<32>(src, out) }
pub fn lex_64<'out>(src: &[u8], out: &'out mut [u8]) -> &'out mut [u8] { lex::<64>(src, out) }

pub fn lex<'out, const VEC_LEN: usize>(input: &[u8], out: &'out mut [u8]) -> &'out mut [u8] {
    unsafe {
        debug_assert!(input.len() < u32::MAX as usize);
        debug_assert!(input.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
        debug_assert!(out.len() >= input.len() * 5); // Each token is at most 5 bytes (kind + len)
        let out_start = out.as_mut_ptr();
        let src_end = input.as_ptr_range().end.sub(VEC_LEN * 2);
        let out_end = lex_loop::<VEC_LEN>(input.as_ptr(), src_end, out_start);
        let out_len = out_end.offset_from_unsigned(out_start);
        &mut out[..out_len]
    }
}

#[must_use]
unsafe fn write_token(out: *mut u8, kind: TokenKind, start: *const u8, end: *const u8) -> *mut u8 {
    unsafe {
        let len = end.offset_from_unsigned(start) as u32;
        debug_assert_ne!(len, 0);
        let out = write_and_advance(out, kind as u8);
        write_and_advance(out, len)
    }
}

#[must_use]
unsafe fn write_punct(out: *mut u8, kind: u8) -> *mut u8 { unsafe { write_and_advance(out, kind) } }

pub fn prepare_input<const VEC_LEN: usize>(src: &str) -> Vec<u8> {
    unsafe {
        let size = src.len() + VEC_LEN * 2;
        let layout = std::alloc::Layout::from_size_align(size, VEC_LEN).unwrap();
        let ptr = std::alloc::alloc(layout);
        assert!(!ptr.is_null());
        let mut vec = Vec::from_raw_parts(ptr, 0, size);
        vec.extend(src.as_bytes());
        vec.extend([EOF_BYTE; VEC_LEN]);
        vec.extend([EOF_BYTE; VEC_LEN]);
        assert!(vec.as_ptr().is_aligned_to(VEC_LEN));
        vec
    }
}

const fn is_punct(b: u8) -> bool {
    #[allow(clippy::match_like_matches_macro)]
    match b {
        | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-' | b'*'
        | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.' | b'!' | b'>'
        | b'<' | b'^' => true,
        _ => false,
    }
}

fn newline_bitstring<const VEC_LEN: usize>(vec: Simd<u8, VEC_LEN>) -> BitString<VEC_LEN> {
    let mask = eq(vec, b'\n');
    BitString::<VEC_LEN>::new(movemask(mask).reverse_bits())
}

fn lex_loop<const VEC_LEN: usize>(
    mut src: *const u8,
    src_end: *const u8,
    mut out: *mut u8,
) -> *mut u8 {
    unsafe {
        loop {
            let token_start = src;
            let byte = src.read();
            debug_assert!(src <= src_end.add(VEC_LEN));

            match byte {
                | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-'
                | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.'
                | b'!' | b'>' | b'<' | b'^' => (src, out) = lex_punct(src, out, byte),

                b' ' | b'\n' | b'\t' => (src, out) = lex_whitespace(src, out),

                b'/' => match src.add(1).read() {
                    b'/' => (src, out) = lex_line_comment::<VEC_LEN>(src, src_end, out),
                    b'*' => (src, out) = lex_block_comment(src, out),
                    _ => {
                        src = src.add(1);
                        out = write_punct(out, byte);
                    }
                },

                b'\'' => (src, out) = lex_lifetime_or_char(src, out),
                b'"' => (src, out) = lex_string(src, src, out, TokenKind::Str),

                b'b' | b'c' | b'r' => match src.cast::<[u8; 3]>().read() {
                    [b'b', b'\'', _] => (src, out) = lex_b_char(src, out),
                    [b'b', b'r', b'"'] => {
                        (src, out) = lex_raw_string(src, src.add(2), out, TokenKind::RawByteStr);
                    }
                    [b'b', b'r', b'#'] => {
                        (src, out) = lex_hash_string(src, src.add(2), out, TokenKind::RawByteStr);
                    }
                    [b'b', b'"', _] => {
                        (src, out) = lex_string(src, src.add(1), out, TokenKind::ByteStr);
                    }

                    [b'c', b'r', b'"'] => {
                        (src, out) = lex_raw_string(src, src.add(2), out, TokenKind::RawCStr);
                    }

                    [b'c', b'r', b'#'] => {
                        (src, out) = lex_hash_string(src, src.add(2), out, TokenKind::RawCStr);
                    }
                    [b'c', b'"', _] => {
                        (src, out) = lex_string(src, src.add(1), out, TokenKind::CStr);
                    }

                    [b'r', b'"', _] => {
                        (src, out) = lex_raw_string(src, src.add(1), out, TokenKind::RawStr);
                    }
                    [b'r', b'#', b'#' | b'"'] => {
                        (src, out) = lex_hash_string(src, src.add(1), out, TokenKind::RawStr);
                    }
                    [b'r', b'#', _] => (src, out) = lex_raw_ident(src, out),
                    _ => (src, out) = lex_ident(src, out),
                },

                b'_' | b'a'..=b'z' | b'A'..=b'Z' => (src, out) = lex_ident(src, out),
                b'0'..=b'9' => (src, out) = lex_int_or_float(src, out),

                EOF_BYTE => return out,
                _ => {
                    src = src.add(1);
                    out = write_token(out, TokenKind::Unknown, token_start, src);
                }
            }
        }
    }
}

unsafe fn lex_punct(mut src: *const u8, mut out: *mut u8, mut byte: u8) -> (*const u8, *mut u8) {
    unsafe {
        loop {
            out = write_punct(out, byte);
            src = src.add(1);
            byte = src.read();
            if !is_punct(byte) {
                break;
            }
        }
        (src, out)
    }
}

unsafe fn lex_whitespace(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        while let b' ' | b'\n' | b'\t' = src.read() {
            src = src.add(1);
        }
        out = write_token(out, TokenKind::Whitespace, token_start, src);
        (src, out)
    }
}

unsafe fn lex_line_comment<const VEC_LEN: usize>(
    mut src: *const u8,
    src_end: *const u8,
    mut out: *mut u8,
) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;

        let mut chunk_ptr = align_down::<VEC_LEN>(src);
        let mut chunk_offset = src.offset_from_unsigned(chunk_ptr);
        let mut vec = load::<VEC_LEN>(chunk_ptr);
        let mut newlines = newline_bitstring(vec);

        loop {
            let len = (newlines << chunk_offset).leading_zeros();
            if len + chunk_offset < VEC_LEN {
                src = chunk_ptr.add(len + chunk_offset);
                out = write_token(out, TokenKind::LineComment, token_start, src);
                return (src, out);
            }

            chunk_offset = 0;
            chunk_ptr = chunk_ptr.add(VEC_LEN);
            if chunk_ptr >= src_end {
                src = src_end;
                out = write_token(out, TokenKind::LineComment, token_start, src);
                return (src, out);
            }
            vec = load::<VEC_LEN>(chunk_ptr);
            newlines = newline_bitstring(vec);
        }
    }
}

unsafe fn lex_block_comment(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        let mut depth = 0u32;

        loop {
            match &src.cast::<[u8; 2]>().read() {
                b"/*" => {
                    src = src.add(2);
                    depth += 1;
                }
                b"*/" => {
                    src = src.add(2);
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                [EOF_BYTE, _] => break,
                _ => src = src.add(1),
            }
        }

        out = write_token(out, TokenKind::BlockComment, token_start, src);
        (src, out)
    }
}

unsafe fn lex_raw_ident(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        debug_assert_eq!(src.cast::<[u8; 2]>().read(), *b"r#");
        src = src.add(2);

        while let b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' = src.read() {
            src = src.add(1);
        }
        out = write_token(out, TokenKind::RawIdent, token_start, src);
        (src, out)
    }
}

unsafe fn lex_ident(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        while let b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' = src.read() {
            src = src.add(1);
        }
        out = write_token(out, TokenKind::Ident, token_start, src);
        (src, out)
    }
}

unsafe fn lex_int_or_float(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        while let b'_' | b'0'..=b'9' = src.read() {
            src = src.add(1);
        }
        let mut kind = match src.cast::<[u8; 2]>().read() {
            [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_'] => {
                out = write_token(out, TokenKind::Int, token_start, src);
                return (src, out);
            }
            [b'.', _] => {
                src = src.add(1);
                while let b'_' | b'0'..=b'9' = src.read() {
                    src = src.add(1);
                }
                TokenKind::Float
            }
            _ => TokenKind::Int,
        };
        if let b'e' | b'E' = src.read() {
            kind = TokenKind::Float;
            src = src.add(1);
            src = src.add(usize::from(matches!(src.read(), b'+' | b'-')));
        }
        while let b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' = src.read() {
            src = src.add(1);
        }
        out = write_token(out, kind, token_start, src);
        (src, out)
    }
}

unsafe fn lex_lifetime_or_char(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        debug_assert_eq!(src.read(), b'\'');
        src = src.add(1);

        if let b'_' | b'a'..=b'z' | b'A'..=b'Z' = src.read() {
            while let b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' = src.read() {
                src = src.add(1);
            }
            if src.read() == b'\'' {
                src = src.add(1);
                out = write_token(out, TokenKind::Char, token_start, src);
                return (src, out);
            }
            out = write_token(out, TokenKind::Lifetime, token_start, src);
            return (src, out);
        }

        loop {
            match src.read() {
                EOF_BYTE => break,
                b'\'' => {
                    let mut src_back = src.sub(1);
                    src = src.add(1);
                    let mut num_backslashes = 0;
                    while src_back.read() == b'\\' {
                        num_backslashes += 1;
                        src_back = src_back.sub(1);
                    }
                    if num_backslashes % 2 == 0 {
                        break;
                    }
                }
                _ => src = src.add(1),
            }
        }

        out = write_token(out, TokenKind::Char, token_start, src);
        (src, out)
    }
}

unsafe fn lex_b_char(mut src: *const u8, mut out: *mut u8) -> (*const u8, *mut u8) {
    unsafe {
        let token_start = src;
        debug_assert_eq!(src.cast::<[u8; 2]>().read(), *b"b'");
        src = src.add(2);

        loop {
            match src.read() {
                EOF_BYTE => break,
                b'\'' => {
                    let mut src_back = src.sub(1);
                    src = src.add(1);
                    let mut num_backslashes = 0;
                    while src_back.read() == b'\\' {
                        num_backslashes += 1;
                        src_back = src_back.sub(1);
                    }
                    if num_backslashes % 2 == 0 {
                        break;
                    }
                }
                _ => src = src.add(1),
            }
        }

        out = write_token(out, TokenKind::Byte, token_start, src);
        (src, out)
    }
}

unsafe fn lex_string(
    token_start: *const u8,
    mut src: *const u8,
    mut out: *mut u8,
    kind: TokenKind,
) -> (*const u8, *mut u8) {
    unsafe {
        debug_assert_eq!(src.read(), b'"');
        src = src.add(1);

        loop {
            match src.read() {
                EOF_BYTE => break,
                b'"' => {
                    let mut src_back = src.sub(1);
                    src = src.add(1);
                    let mut num_backslashes = 0;
                    while src_back.read() == b'\\' {
                        num_backslashes += 1;
                        src_back = src_back.sub(1);
                    }
                    if num_backslashes % 2 == 0 {
                        break;
                    }
                }
                _ => src = src.add(1),
            }
        }

        out = write_token(out, kind, token_start, src);
        (src, out)
    }
}

unsafe fn lex_hash_string(
    token_start: *const u8,
    mut src: *const u8,
    mut out: *mut u8,
    kind: TokenKind,
) -> (*const u8, *mut u8) {
    unsafe {
        debug_assert_eq!(src.read(), b'#');

        let mut opening_hashes = 0u32;
        while src.read() == b'#' {
            opening_hashes += 1;
            src = src.add(1);
        }

        let b'"' = src.read() else {
            out = write_token(out, kind, token_start, src);
            return (src, out);
        };
        src = src.add(1);

        'outer: loop {
            match src.read() {
                EOF_BYTE => break,
                b'"' => {
                    src = src.add(1);
                    let mut closing_hashes = 0u32;
                    while src.read() == b'#' {
                        closing_hashes += 1;
                        src = src.add(1);
                        if closing_hashes == opening_hashes {
                            break 'outer;
                        }
                    }
                }
                _ => src = src.add(1),
            }
        }

        out = write_token(out, kind, token_start, src);
        (src, out)
    }
}

unsafe fn lex_raw_string(
    token_start: *const u8,
    mut src: *const u8,
    mut out: *mut u8,
    kind: TokenKind,
) -> (*const u8, *mut u8) {
    unsafe {
        debug_assert_eq!(src.read(), b'"');
        src = src.add(1);

        loop {
            match src.read() {
                EOF_BYTE => break,
                b'"' => {
                    src = src.add(1);
                    break;
                }
                _ => src = src.add(1),
            }
        }

        out = write_token(out, kind, token_start, src);
        (src, out)
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
        let input = prepare_input::<VEC_LEN>(src);
        let mut buf = vec![EOF_BYTE; input.len() * 10];
        let buf = lex::<VEC_LEN>(&input, &mut buf);
        let mut buf = buf.iter().copied();

        let mut pos = 0usize;
        let mut decoded = Vec::new();
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
            let end = pos + len as usize;
            decoded.push((kind, pos..end));
            pos = end;
        }

        let mut out = String::new();
        for (kind, range) in decoded {
            let lexeme = &src[range.clone()];
            _ = writeln!(out, "({kind:?}, {range:?}, {lexeme:?})");
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
            r###"
            r#""#
            r##""##
            r#"raw string""""""""#
            r#"""""""""#
            r##" ##"" "##
            r#"unterminated" "###,
            &expect![[r###"
                (Whitespace, 0..13, "\n            ")
                (RawStr, 13..18, "r#\"\"#")
                (Whitespace, 18..31, "\n            ")
                (RawStr, 31..38, "r##\"\"##")
                (Whitespace, 38..51, "\n            ")
                (RawStr, 51..73, "r#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 73..86, "\n            ")
                (RawStr, 86..98, "r#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 98..111, "\n            ")
                (RawStr, 111..124, "r##\" ##\"\" \"##")
                (Whitespace, 124..137, "\n            ")
                (RawStr, 137..154, "r#\"unterminated\" ")
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
