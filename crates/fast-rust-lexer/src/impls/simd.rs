#![allow(clippy::wildcard_imports)]

use std::bstr::ByteStr;
use std::debug_assert_matches;
use std::simd::Simd;

use crate::TokenKind;
use crate::utils::simdx::*;
use crate::utils::*;

pub const EOF_BYTE: u8 = 0xff;

pub fn prepare_input<const VEC_LEN: usize>(src: &str) -> Vec<u8> {
    unsafe {
        let size = src.len() + VEC_LEN;
        let layout = std::alloc::Layout::from_size_align(size, VEC_LEN).unwrap();
        let ptr = std::alloc::alloc(layout);
        assert!(!ptr.is_null());
        let mut padded_input = Vec::from_raw_parts(ptr, 0, size);
        padded_input.extend(src.as_bytes());
        padded_input.extend([EOF_BYTE; VEC_LEN]);
        assert!(padded_input.as_ptr().is_aligned_to(VEC_LEN));

        padded_input
    }
}

#[derive(Default)]
struct Carry {
    whitespace: u64,
    ident:      u64,
    slash:      u64,
}

struct Chunk {
    whitespace: u64,
    idents:     u64,
    puncts:     u64,
    newlines:   u64,
}

fn mask_starts<const VEC_LEN: usize>(mask: u64, carry: &mut u64) -> u64 {
    debug_assert_matches!(*carry, 1 | 0);

    let starts = mask & !(mask << 1) & !*carry;
    *carry = mask >> (VEC_LEN - 1);
    debug_assert_matches!(*carry, 1 | 0);

    starts
}

#[inline]
fn get_chunk<const VEC_LEN: usize>(vec: Simd<u8, VEC_LEN>, carry: &mut Carry) -> Chunk {
    let ws_chars = movemask(in_range(vec, 0x09, 0x0D) | eq(vec, b' '));
    let ident_chars = movemask(
        eq(vec, b'_')
            | in_range(vec, b'a', b'z')
            | in_range(vec, b'A', b'Z')
            | in_range(vec, b'0', b'9'),
    );
    let newlines = movemask(eq(vec, b'\n'));

    deprintln!("vec           = {}", ByteStr::new(vec.as_array()));
    deprintln!("ws_chars      = {}", fmt_bitmask::<VEC_LEN>(ws_chars));
    deprintln!("newlines      = {}", fmt_bitmask::<VEC_LEN>(newlines));
    deprintln!("ident_chars   = {}", fmt_bitmask::<VEC_LEN>(ident_chars));

    let whitespace = mask_starts::<VEC_LEN>(ws_chars, &mut carry.whitespace);
    deprintln!("ws_starts     = {}", fmt_bitmask::<VEC_LEN>(whitespace));

    let idents = mask_starts::<VEC_LEN>(ident_chars, &mut carry.ident);
    deprintln!("ident_starts  = {}", fmt_bitmask::<VEC_LEN>(idents));

    // Any character that is not an alphanumeric char or a whitespace char is a
    // punctuation char.
    let puncts = !ws_chars & !ident_chars;
    deprintln!("punct         = {}", fmt_bitmask::<VEC_LEN>(puncts));

    let normal_starts = idents | puncts | whitespace;
    deprintln!("normal_starts = {}", fmt_bitmask::<VEC_LEN>(normal_starts));
    deprintln!("vec           = {}", ByteStr::new(vec.as_array()));

    let slash = movemask(eq(vec, b'/'));
    let star = movemask(eq(vec, b'*'));

    let next_slash = slash >> 1;
    let next_star = star >> 1;

    let line_comments = (carry.slash | (slash << 1)) & slash; // `//`
    let block_comments = (carry.slash | (slash << 1)) & star; // `/*`

    deprintln!();
    deprintln!("vec      = {}", ByteStr::new(&vec));
    deprintln!("*        = {}", fmt_bitmask::<VEC_LEN>(star));
    deprintln!("* >> 1   = {}", fmt_bitmask::<VEC_LEN>(next_star));
    deprintln!("/ carry  = {}", fmt_bitmask::<VEC_LEN>(carry.slash));
    deprintln!("/        = {}", fmt_bitmask::<VEC_LEN>(slash));
    deprintln!("/ >> 1   = {}", fmt_bitmask::<VEC_LEN>(next_slash));
    deprintln!("//       = {}", fmt_bitmask::<VEC_LEN>(line_comments));
    deprintln!("/*       = {}", fmt_bitmask::<VEC_LEN>(block_comments));
    deprintln!();

    carry.slash = slash >> (VEC_LEN - 1);

    Chunk {
        whitespace,
        idents,
        puncts,
        newlines,
    }
}

pub fn lex<const VEC_LEN: usize>(padded_input: &[u8], mut on_token: impl FnMut(TokenKind, u32)) {
    unsafe {
        debug_assert!(padded_input.ends_with(&[EOF_BYTE; VEC_LEN]));

        let src_start = padded_input.as_ptr();
        let padded_src_end = padded_input.as_ptr_range().end;
        let real_src_end = padded_src_end.sub(VEC_LEN);

        let mut src_ptr = src_start;
        let mut carry = Carry::default();

        'outer: loop {
            let vec = load::<VEC_LEN>(src_ptr);
            let mut chunk = get_chunk(vec, &mut carry);
            let token_starts = chunk.idents | chunk.puncts | chunk.whitespace;
            let mut token_starts = match VEC_LEN {
                16 => token_starts & 0xffff,
                32 => token_starts & 0xffff_ffff,
                64 => token_starts,
                _ => unreachable!(),
            };

            while token_starts != 0 {
                let tz = (token_starts.trailing_zeros() as usize).min(VEC_LEN);
                let token_start_ptr = src_ptr.add(tz);
                let token_start_pos = token_start_ptr.offset_from_unsigned(src_start) as u32;
                let bytes = token_start_ptr.cast::<[u8; 4]>().read();
                token_starts &= !token_starts.isolate_lowest_one();

                let kind = match bytes {
                    [b'/', b'/', ..] => {
                        let newlines = chunk.newlines & token_starts;

                        if newlines == 0 {
                            let mut token_end_ptr = token_start_ptr.add(2);
                            while token_end_ptr.read() != b'\n' && token_end_ptr.read() != EOF_BYTE
                            {
                                token_end_ptr = token_end_ptr.add(1);
                            }
                            on_token(TokenKind::LineComment, token_start_pos);
                            src_ptr = token_end_ptr;
                            carry = Carry::default();
                            continue 'outer;
                        }

                        let lowest_one = newlines.isolate_lowest_one();
                        token_starts &= !(lowest_one - 1);
                        chunk.newlines &= !(lowest_one - 1);

                        TokenKind::LineComment
                    }
                    [b'/', b'*', ..] => {
                        let mut depth = 1u32;
                        let mut token_end_ptr = token_start_ptr.add(2);
                        loop {
                            match token_end_ptr.cast::<[u8; 2]>().read() {
                                [b'/', b'*'] => {
                                    depth += 1;
                                    token_end_ptr = token_end_ptr.add(2);
                                }
                                [b'*', b'/'] => {
                                    depth -= 1;
                                    token_end_ptr = token_end_ptr.add(2);
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                [EOF_BYTE, _] => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                        on_token(TokenKind::BlockComment, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }

                    [b'"', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(1);
                        token_end_ptr = loop {
                            match token_end_ptr.read() {
                                b'"' => break token_end_ptr.add(1),
                                EOF_BYTE => break token_end_ptr,

                                b'\\' => token_end_ptr = token_end_ptr.add(2),
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        };
                        on_token(TokenKind::Str, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'b', b'"', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(2);
                        token_end_ptr = loop {
                            match token_end_ptr.read() {
                                b'"' => break token_end_ptr.add(1),
                                EOF_BYTE => break token_end_ptr,

                                b'\\' => token_end_ptr = token_end_ptr.add(2),
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        };
                        on_token(TokenKind::ByteStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'c', b'"', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(2);
                        token_end_ptr = loop {
                            match token_end_ptr.read() {
                                b'"' => break token_end_ptr.add(1),
                                EOF_BYTE => break token_end_ptr,

                                b'\\' => token_end_ptr = token_end_ptr.add(2),
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        };
                        on_token(TokenKind::CStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }

                    [b'r', b'"', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(2);
                        loop {
                            match token_end_ptr.read() {
                                b'"' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    break;
                                }
                                EOF_BYTE => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                        on_token(TokenKind::RawStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'b', b'r', b'"', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(3);
                        loop {
                            match token_end_ptr.read() {
                                b'"' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    break;
                                }
                                EOF_BYTE => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                        on_token(TokenKind::RawByteStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'c', b'r', b'"', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(3);
                        loop {
                            match token_end_ptr.read() {
                                b'"' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    break;
                                }
                                EOF_BYTE => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                        on_token(TokenKind::RawCStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }

                    [b'r', b'#', b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(2);
                        while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' =
                            token_end_ptr.read()
                        {
                            token_end_ptr = token_end_ptr.add(1);
                        }
                        on_token(TokenKind::RawIdent, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'r', b'#', ..] => {
                        let mut hash_count = 0u32;
                        let mut token_end_ptr = token_start_ptr.add(1);
                        while token_end_ptr.read() == b'#' {
                            hash_count += 1;
                            token_end_ptr = token_end_ptr.add(1);
                        }
                        if token_end_ptr.read() == b'"' {
                            token_end_ptr = token_end_ptr.add(1);

                            'foo: loop {
                                match token_end_ptr.read() {
                                    b'"' => {
                                        token_end_ptr = token_end_ptr.add(1);
                                        let mut hash_count = hash_count;
                                        while token_end_ptr.read() == b'#' {
                                            token_end_ptr = token_end_ptr.add(1);
                                            hash_count -= 1;
                                            if hash_count == 0 {
                                                break 'foo;
                                            }
                                        }
                                    }
                                    EOF_BYTE => break,
                                    _ => token_end_ptr = token_end_ptr.add(1),
                                }
                            }
                        }
                        on_token(TokenKind::RawStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'b', b'r', b'#', ..] => {
                        let mut hash_count = 0u32;
                        let mut token_end_ptr = token_start_ptr.add(2);
                        while token_end_ptr.read() == b'#' {
                            hash_count += 1;
                            token_end_ptr = token_end_ptr.add(1);
                        }
                        if token_end_ptr.read() == b'"' {
                            token_end_ptr = token_end_ptr.add(1);

                            'foo: loop {
                                match token_end_ptr.read() {
                                    b'"' => {
                                        token_end_ptr = token_end_ptr.add(1);
                                        let mut hash_count = hash_count;
                                        while token_end_ptr.read() == b'#' {
                                            token_end_ptr = token_end_ptr.add(1);
                                            hash_count -= 1;
                                            if hash_count == 0 {
                                                break 'foo;
                                            }
                                        }
                                    }
                                    EOF_BYTE => break,
                                    _ => token_end_ptr = token_end_ptr.add(1),
                                }
                            }
                        }
                        on_token(TokenKind::RawByteStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'c', b'r', b'#', ..] => {
                        let mut hash_count = 0u32;
                        let mut token_end_ptr = token_start_ptr.add(2);
                        while token_end_ptr.read() == b'#' {
                            hash_count += 1;
                            token_end_ptr = token_end_ptr.add(1);
                        }
                        if token_end_ptr.read() == b'"' {
                            token_end_ptr = token_end_ptr.add(1);

                            'foo: loop {
                                match token_end_ptr.read() {
                                    b'"' => {
                                        token_end_ptr = token_end_ptr.add(1);
                                        let mut hash_count = hash_count;
                                        while token_end_ptr.read() == b'#' {
                                            token_end_ptr = token_end_ptr.add(1);
                                            hash_count -= 1;
                                            if hash_count == 0 {
                                                break 'foo;
                                            }
                                        }
                                    }
                                    EOF_BYTE => break,
                                    _ => token_end_ptr = token_end_ptr.add(1),
                                }
                            }
                        }
                        on_token(TokenKind::RawCStr, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }

                    [b'\'', b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(2);
                        loop {
                            match token_end_ptr.read() {
                                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                }
                                b'\'' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    on_token(TokenKind::Char, token_start_pos);
                                    src_ptr = token_end_ptr;
                                    carry = Carry::default();
                                    continue 'outer;
                                }
                                _ => {
                                    on_token(TokenKind::Lifetime, token_start_pos);
                                    src_ptr = token_end_ptr;
                                    carry = Carry::default();
                                    continue 'outer;
                                }
                            }
                        }
                    }
                    [b'\'', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(1);
                        token_end_ptr = loop {
                            match token_end_ptr.read() {
                                b'\'' => break token_end_ptr.add(1),
                                EOF_BYTE => break token_end_ptr,

                                b'\\' => token_end_ptr = token_end_ptr.add(2),
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        };

                        on_token(TokenKind::Char, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }
                    [b'b', b'\'', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(2);

                        loop {
                            match token_end_ptr.read() {
                                b'\\' => token_end_ptr = token_end_ptr.add(2),
                                b'\'' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    break;
                                }
                                EOF_BYTE => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }

                        on_token(TokenKind::Byte, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }

                    [b'0'..=b'9', ..] => {
                        let mut token_end_ptr = token_start_ptr.add(1);
                        while let b'0'..=b'9' | b'_' = token_end_ptr.read() {
                            token_end_ptr = token_end_ptr.add(1);
                        }
                        let mut kind;
                        match token_end_ptr.cast::<[u8; 2]>().read() {
                            [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_'] => {
                                on_token(TokenKind::Int, token_start_pos);
                                src_ptr = token_end_ptr;
                                carry = Carry::default();
                                continue 'outer;
                            }
                            [b'.', _] => {
                                token_end_ptr = token_end_ptr.add(1);
                                while let b'0'..=b'9' | b'_' = token_end_ptr.read() {
                                    token_end_ptr = token_end_ptr.add(1);
                                }
                                kind = TokenKind::Float;
                            }
                            _ => kind = TokenKind::Int,
                        }

                        if let b'e' | b'E' = token_end_ptr.read() {
                            token_end_ptr = token_end_ptr.add(1);
                            kind = TokenKind::Float;

                            if let b'+' | b'-' = token_end_ptr.read() {
                                token_end_ptr = token_end_ptr.add(1);
                            }
                        }

                        while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' =
                            token_end_ptr.read()
                        {
                            token_end_ptr = token_end_ptr.add(1);
                        }

                        on_token(kind, token_start_pos);
                        src_ptr = token_end_ptr;
                        carry = Carry::default();
                        continue 'outer;
                    }

                    [b'/', ..] => TokenKind::Slash,
                    [b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => TokenKind::Ident,
                    [b' ' | 0x0a..=0x0d, ..] => TokenKind::Whitespace,
                    [
                        b @ (b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+'
                        | b'-' | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#'
                        | b'@' | b'.' | b'!' | b'>' | b'<' | b'^'),
                        ..,
                    ] => TokenKind::from_u8(b).unwrap_unchecked(),
                    [EOF_BYTE, ..] => TokenKind::Eof,
                    _ => TokenKind::Unknown,
                };
                on_token(kind, token_start_pos);
            }

            src_ptr = src_ptr.add(VEC_LEN);
            if src_ptr >= real_src_end {
                break 'outer;
            }
        }
    }
    let eof_pos = padded_input.len() - VEC_LEN;
    on_token(TokenKind::Eof, eof_pos as u32);
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;

    const VEC_LEN: usize = 16;

    #[track_caller]
    fn check(input: &str, expect: &Expect) {
        use std::fmt::Write;

        let padded_input = prepare_input::<VEC_LEN>(input);
        let mut tokens = Vec::new();
        lex::<VEC_LEN>(&padded_input, |kind, pos| tokens.push((kind, pos)));

        let mut actual = String::new();
        let mut iter = tokens.iter().copied();
        let (mut kind, mut start) = iter.next().unwrap();

        for (next_kind, next_start) in iter {
            let range = start..next_start;
            let lexeme = ByteStr::new(&padded_input[start as usize..next_start as usize]);

            _ = writeln!(actual, "({kind:?}, {range:?}, «{lexeme}»)");
            kind = next_kind;
            start = next_start;

            if start >= input.len() as u32 {
                break;
            }
        }

        expect.assert_eq(&actual);
    }

    #[test]
    fn empty() {
        check("", &expect![[r"
            (Eof, 0..1, «�»)
        "]]);
    }

    #[test]
    fn idents() {
        check("a", &expect![[r"
            (Ident, 0..1, «a»)
        "]]);
        check("abc123", &expect![[r"
            (Ident, 0..6, «abc123»)
        "]]);
        check("_", &expect![[r"
            (Ident, 0..1, «_»)
        "]]);
        check("abc_123_", &expect![[r"
            (Ident, 0..8, «abc_123_»)
        "]]);
        check("abcdef123456789", &expect![[r"
            (Ident, 0..15, «abcdef123456789»)
        "]]);

        check("abcdef1234567890", &expect![[r"
            (Ident, 0..16, «abcdef1234567890»)
        "]]);
    }

    #[test]
    fn idents2() {
        check("abcdef1234567890xyz", &expect![[r"
            (Ident, 0..19, «abcdef1234567890xyz»)
        "]]);
    }

    #[test]
    fn idents_and_whitespace() {
        check("a b c", &expect![[r"
            (Ident, 0..1, «a»)
            (Whitespace, 1..2, « »)
            (Ident, 2..3, «b»)
            (Whitespace, 3..4, « »)
            (Ident, 4..5, «c»)
        "]]);

        check("abc  def  ghi", &expect![[r"
            (Ident, 0..3, «abc»)
            (Whitespace, 3..5, «  »)
            (Ident, 5..8, «def»)
            (Whitespace, 8..10, «  »)
            (Ident, 10..13, «ghi»)
        "]]);
    }

    #[test]
    fn punctuation() {
        check("!#$%&()*+,-./:;<=>?[]^{|}~", &expect![[r"
            (Bang, 0..1, «!»)
            (Hash, 1..2, «#»)
            (Dollar, 2..3, «$»)
            (Percent, 3..4, «%»)
            (Ampersand, 4..5, «&»)
            (LParen, 5..6, «(»)
            (RParen, 6..7, «)»)
            (Star, 7..8, «*»)
            (Plus, 8..9, «+»)
            (Comma, 9..10, «,»)
            (Minus, 10..11, «-»)
            (Dot, 11..12, «.»)
            (Slash, 12..13, «/»)
            (Colon, 13..14, «:»)
            (Semicolon, 14..15, «;»)
            (Lt, 15..16, «<»)
            (Eq, 16..17, «=»)
            (Gt, 17..18, «>»)
            (Question, 18..19, «?»)
            (LSquare, 19..20, «[»)
            (RSquare, 20..21, «]»)
            (Caret, 21..22, «^»)
            (LCurly, 22..23, «{»)
            (Bar, 23..24, «|»)
            (RCurly, 24..25, «}»)
            (Tilde, 25..26, «~»)
        "]]);
        check("!#///*\n", &expect![[r"
            (Bang, 0..1, «!»)
            (Hash, 1..2, «#»)
            (LineComment, 2..6, «///*»)
            (Whitespace, 6..7, «
            »)
        "]]);
        check("!#/*\n*/~>", &expect![[r"
            (Bang, 0..1, «!»)
            (Hash, 1..2, «#»)
            (BlockComment, 2..7, «/*
            */»)
            (Tilde, 7..8, «~»)
            (Gt, 8..9, «>»)
        "]]);
    }

    #[test]
    fn strings() {
        check(
            r#"{ "\\\"Nam[": [ 116,"\\\\" , 234 , "true" , false ] , "t" : "\\\"" }"#,
            &expect![[r#"
                (LCurly, 0..1, «{»)
                (Whitespace, 1..2, « »)
                (Str, 2..12, «"\\\"Nam["»)
                (Colon, 12..13, «:»)
                (Whitespace, 13..14, « »)
                (LSquare, 14..15, «[»)
                (Whitespace, 15..16, « »)
                (Int, 16..19, «116»)
                (Comma, 19..20, «,»)
                (Str, 20..26, «"\\\\"»)
                (Whitespace, 26..27, « »)
                (Comma, 27..28, «,»)
                (Whitespace, 28..29, « »)
                (Int, 29..32, «234»)
                (Whitespace, 32..33, « »)
                (Comma, 33..34, «,»)
                (Whitespace, 34..35, « »)
                (Str, 35..41, «"true"»)
                (Whitespace, 41..42, « »)
                (Comma, 42..43, «,»)
                (Whitespace, 43..44, « »)
                (Ident, 44..49, «false»)
                (Whitespace, 49..50, « »)
                (RSquare, 50..51, «]»)
                (Whitespace, 51..52, « »)
                (Comma, 52..53, «,»)
                (Whitespace, 53..54, « »)
                (Str, 54..57, «"t"»)
                (Whitespace, 57..58, « »)
                (Colon, 58..59, «:»)
                (Whitespace, 59..60, « »)
                (Str, 60..66, «"\\\""»)
                (Whitespace, 66..67, « »)
                (RCurly, 67..68, «}»)
            "#]],
        );

        check(
            r#"{ b"\\\"Nam[": [ 116,b"\\\\" , 234 , b"true" , false ] , b"t" : b"\\\"" }"#,
            &expect![[r#"
                (LCurly, 0..1, «{»)
                (Whitespace, 1..2, « »)
                (ByteStr, 2..13, «b"\\\"Nam["»)
                (Colon, 13..14, «:»)
                (Whitespace, 14..15, « »)
                (LSquare, 15..16, «[»)
                (Whitespace, 16..17, « »)
                (Int, 17..20, «116»)
                (Comma, 20..21, «,»)
                (ByteStr, 21..28, «b"\\\\"»)
                (Whitespace, 28..29, « »)
                (Comma, 29..30, «,»)
                (Whitespace, 30..31, « »)
                (Int, 31..34, «234»)
                (Whitespace, 34..35, « »)
                (Comma, 35..36, «,»)
                (Whitespace, 36..37, « »)
                (ByteStr, 37..44, «b"true"»)
                (Whitespace, 44..45, « »)
                (Comma, 45..46, «,»)
                (Whitespace, 46..47, « »)
                (Ident, 47..52, «false»)
                (Whitespace, 52..53, « »)
                (RSquare, 53..54, «]»)
                (Whitespace, 54..55, « »)
                (Comma, 55..56, «,»)
                (Whitespace, 56..57, « »)
                (ByteStr, 57..61, «b"t"»)
                (Whitespace, 61..62, « »)
                (Colon, 62..63, «:»)
                (Whitespace, 63..64, « »)
                (ByteStr, 64..71, «b"\\\""»)
                (Whitespace, 71..72, « »)
                (RCurly, 72..73, «}»)
            "#]],
        );

        check(
            r#"{ c"\\\"Nam[": [ 116,c"\\\\" , 234 , c"true" , false ] , c"t" : c"\\\"" }"#,
            &expect![[r#"
                (LCurly, 0..1, «{»)
                (Whitespace, 1..2, « »)
                (CStr, 2..13, «c"\\\"Nam["»)
                (Colon, 13..14, «:»)
                (Whitespace, 14..15, « »)
                (LSquare, 15..16, «[»)
                (Whitespace, 16..17, « »)
                (Int, 17..20, «116»)
                (Comma, 20..21, «,»)
                (CStr, 21..28, «c"\\\\"»)
                (Whitespace, 28..29, « »)
                (Comma, 29..30, «,»)
                (Whitespace, 30..31, « »)
                (Int, 31..34, «234»)
                (Whitespace, 34..35, « »)
                (Comma, 35..36, «,»)
                (Whitespace, 36..37, « »)
                (CStr, 37..44, «c"true"»)
                (Whitespace, 44..45, « »)
                (Comma, 45..46, «,»)
                (Whitespace, 46..47, « »)
                (Ident, 47..52, «false»)
                (Whitespace, 52..53, « »)
                (RSquare, 53..54, «]»)
                (Whitespace, 54..55, « »)
                (Comma, 55..56, «,»)
                (Whitespace, 56..57, « »)
                (CStr, 57..61, «c"t"»)
                (Whitespace, 61..62, « »)
                (Colon, 62..63, «:»)
                (Whitespace, 63..64, « »)
                (CStr, 64..71, «c"\\\""»)
                (Whitespace, 71..72, « »)
                (RCurly, 72..73, «}»)
            "#]],
        );

        check(r#""0123456789012""after""#, &expect![[r#"
            (Str, 0..15, «"0123456789012"»)
            (Str, 15..22, «"after"»)
        "#]]);
        check(r#""01234567890123""after""#, &expect![[r#"
            (Str, 0..16, «"01234567890123"»)
            (Str, 16..23, «"after"»)
        "#]]);
        check(r#""012345678901234""after""#, &expect![[r#"
            (Str, 0..17, «"012345678901234"»)
            (Str, 17..24, «"after"»)
        "#]]);
    }

    #[test]
    fn unterminated_strings() {
        check(r#""unterminated"#, &expect![[r#"
            (Str, 0..13, «"unterminated»)
        "#]]);

        check(r#""unterminated over several chunks"#, &expect![[r#"
            (Str, 0..33, «"unterminated over several chunks»)
        "#]]);
    }

    #[test]
    fn line_comments() {
        check("// foo\n//line comment EOF", &expect![[r"
            (LineComment, 0..6, «// foo»)
            (Whitespace, 6..7, «
            »)
            (LineComment, 7..25, «//line comment EOF»)
        "]]);

        check("_123456789abcdef//", &expect![[r"
        (Ident, 0..16, «_123456789abcdef»)
        (LineComment, 16..18, «//»)
        "]]);

        check("_123456789abcde//", &expect![[r"
            (Ident, 0..15, «_123456789abcde»)
            (LineComment, 15..17, «//»)
        "]]);

        check("_123456789abcdefg//", &expect![[r"
            (Ident, 0..17, «_123456789abcdefg»)
            (LineComment, 17..19, «//»)
        "]]);
    }

    #[test]
    fn block_comments() {
        check(
            "/* block comment */
            /* nested block comment /* still nested */ also still nested */
            /* /* unclosed block comment */
            oh no, still in a comment",
            &expect![[r"
                (BlockComment, 0..19, «/* block comment */»)
                (Whitespace, 19..32, «
                            »)
                (BlockComment, 32..95, «/* nested block comment /* still nested */ also still nested */»)
                (Whitespace, 95..108, «
                            »)
                (BlockComment, 108..177, «/* /* unclosed block comment */
                            oh no, still in a comment»)
            "]],
        );

        check("/* EOF", &expect![[r"
            (BlockComment, 0..6, «/* EOF»)
        "]]);
        check("/*/ EOF", &expect![[r"
            (BlockComment, 0..7, «/*/ EOF»)
        "]]);
    }

    #[test]
    fn block_comments2() {
        check("/**/ EOF", &expect![[r"
            (BlockComment, 0..4, «/**/»)
            (Whitespace, 4..5, « »)
            (Ident, 5..8, «EOF»)
        "]]);
        check("/*// EOF", &expect![[r"
            (BlockComment, 0..8, «/*// EOF»)
        "]]);
        check("/*/* EOF", &expect![[r"
            (BlockComment, 0..8, «/*/* EOF»)
        "]]);
        check("/*/**/ EOF", &expect![[r"
            (BlockComment, 0..10, «/*/**/ EOF»)
        "]]);
        check("/*/**/ EOF", &expect![[r"
            (BlockComment, 0..10, «/*/**/ EOF»)
        "]]);
        check("/* /* */ */ EOF", &expect![[r"
            (BlockComment, 0..11, «/* /* */ */»)
            (Whitespace, 11..12, « »)
            (Ident, 12..15, «EOF»)
        "]]);
        check("/*/* */ */ EOF", &expect![[r"
            (BlockComment, 0..10, «/*/* */ */»)
            (Whitespace, 10..11, « »)
            (Ident, 11..14, «EOF»)
        "]]);
        check("/*/* */*/ EOF", &expect![[r"
            (BlockComment, 0..9, «/*/* */*/»)
            (Whitespace, 9..10, « »)
            (Ident, 10..13, «EOF»)
        "]]);
        check("/*/* */*/ EOF", &expect![[r"
            (BlockComment, 0..9, «/*/* */*/»)
            (Whitespace, 9..10, « »)
            (Ident, 10..13, «EOF»)
        "]]);
    }

    #[test]
    fn identifiers() {
        check("a abcdefXYZ123 _ _foo _1 __1", &expect![[r"
            (Ident, 0..1, «a»)
            (Whitespace, 1..2, « »)
            (Ident, 2..14, «abcdefXYZ123»)
            (Whitespace, 14..15, « »)
            (Ident, 15..16, «_»)
            (Whitespace, 16..17, « »)
            (Ident, 17..21, «_foo»)
            (Whitespace, 21..22, « »)
            (Ident, 22..24, «_1»)
            (Whitespace, 24..25, « »)
            (Ident, 25..28, «__1»)
        "]]);
    }

    #[test]
    fn numbers() {
        check(
            "0 1234567890 123_456 123suffix 1.2 0.1 0. 0..1 0. 1e 1E 1e+ 1e- 1e+2 1e+2suffix",
            &expect![[r"
                (Int, 0..1, «0»)
                (Whitespace, 1..2, « »)
                (Int, 2..12, «1234567890»)
                (Whitespace, 12..13, « »)
                (Int, 13..20, «123_456»)
                (Whitespace, 20..21, « »)
                (Int, 21..30, «123suffix»)
                (Whitespace, 30..31, « »)
                (Float, 31..34, «1.2»)
                (Whitespace, 34..35, « »)
                (Float, 35..38, «0.1»)
                (Whitespace, 38..39, « »)
                (Float, 39..41, «0.»)
                (Whitespace, 41..42, « »)
                (Int, 42..43, «0»)
                (Dot, 43..44, «.»)
                (Dot, 44..45, «.»)
                (Int, 45..46, «1»)
                (Whitespace, 46..47, « »)
                (Float, 47..49, «0.»)
                (Whitespace, 49..50, « »)
                (Float, 50..52, «1e»)
                (Whitespace, 52..53, « »)
                (Float, 53..55, «1E»)
                (Whitespace, 55..56, « »)
                (Float, 56..59, «1e+»)
                (Whitespace, 59..60, « »)
                (Float, 60..63, «1e-»)
                (Whitespace, 63..64, « »)
                (Float, 64..68, «1e+2»)
                (Whitespace, 68..69, « »)
                (Float, 69..79, «1e+2suffix»)
            "]],
        );

        check(
            "0b10_1010asdfbz 0o755as_dfzxc 0xDEADBE_EFasdfzxc",
            &expect![[r"
                (Int, 0..15, «0b10_1010asdfbz»)
                (Whitespace, 15..16, « »)
                (Int, 16..29, «0o755as_dfzxc»)
                (Whitespace, 29..30, « »)
                (Int, 30..48, «0xDEADBE_EFasdfzxc»)
            "]],
        );
    }

    #[test]
    fn lifetimes() {
        check("'a", &expect![[r"
            (Lifetime, 0..2, «'a»)
        "]]);
        check("'abcdef_1234", &expect![[r"
            (Lifetime, 0..12, «'abcdef_1234»)
        "]]);

        check("'abcdef_1234 foo", &expect![[r"
            (Lifetime, 0..12, «'abcdef_1234»)
            (Whitespace, 12..13, « »)
            (Ident, 13..16, «foo»)
        "]]);

        check("'abcdef_1234'foo", &expect![[r"
            (Char, 0..13, «'abcdef_1234'»)
            (Ident, 13..16, «foo»)
        "]]);
    }

    #[test]
    fn chars() {
        check("'a' '\n' '\\'' '' 'foo'", &expect![[r"
            (Char, 0..3, «'a'»)
            (Whitespace, 3..4, « »)
            (Char, 4..7, «'
            '»)
            (Whitespace, 7..8, « »)
            (Char, 8..12, «'\''»)
            (Whitespace, 12..13, « »)
            (Char, 13..15, «''»)
            (Whitespace, 15..16, « »)
            (Char, 16..21, «'foo'»)
        "]]);

        check("b'a' b'\n' b'\\'' b'' b'foo'", &expect![[r"
            (Byte, 0..4, «b'a'»)
            (Whitespace, 4..5, « »)
            (Byte, 5..9, «b'
            '»)
            (Whitespace, 9..10, « »)
            (Byte, 10..15, «b'\''»)
            (Whitespace, 15..16, « »)
            (Byte, 16..19, «b''»)
            (Whitespace, 19..20, « »)
            (Byte, 20..26, «b'foo'»)
        "]]);
    }

    #[test]
    fn strings2() {
        check(r#""" "simple" "escaped \" quote" "unterminated"#, &expect![
            [r#"
            (Str, 0..2, «""»)
            (Whitespace, 2..3, « »)
            (Str, 3..11, «"simple"»)
            (Whitespace, 11..12, « »)
            (Str, 12..30, «"escaped \" quote"»)
            (Whitespace, 30..31, « »)
            (Str, 31..44, «"unterminated»)
        "#]
        ]);

        check(
            r#"b"" b"simple" b"escaped \" quote" b"unterminated"#,
            &expect![[r#"
                (ByteStr, 0..3, «b""»)
                (Whitespace, 3..4, « »)
                (ByteStr, 4..13, «b"simple"»)
                (Whitespace, 13..14, « »)
                (ByteStr, 14..33, «b"escaped \" quote"»)
                (Whitespace, 33..34, « »)
                (ByteStr, 34..48, «b"unterminated»)
            "#]],
        );

        check(
            r#"c"" c"simple" c"escaped \" quote" c"unterminated"#,
            &expect![[r#"
                (CStr, 0..3, «c""»)
                (Whitespace, 3..4, « »)
                (CStr, 4..13, «c"simple"»)
                (Whitespace, 13..14, « »)
                (CStr, 14..33, «c"escaped \" quote"»)
                (Whitespace, 33..34, « »)
                (CStr, 34..48, «c"unterminated»)
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
                (Whitespace, 0..5, «
                    »)
                (RawStr, 5..19, «r"raw string\"»)
                (Whitespace, 19..24, «
                    »)
                (RawByteStr, 24..39, «br"raw string\"»)
                (Whitespace, 39..44, «
                    »)
                (RawCStr, 44..59, «cr"raw string\"»)
                (Whitespace, 59..64, «
                    »)
                (RawStr, 64..83, «r"unterminated
                    »)
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
    r#"unterminated"
    "###,
            &expect![[r###"
                (Whitespace, 0..5, «
                    »)
                (RawStr, 5..10, «r#""#»)
                (Whitespace, 10..15, «
                    »)
                (RawStr, 15..22, «r##""##»)
                (Whitespace, 22..27, «
                    »)
                (RawStr, 27..49, «r#"raw string""""""""#»)
                (Whitespace, 49..54, «
                    »)
                (RawStr, 54..66, «r#"""""""""#»)
                (Whitespace, 66..71, «
                    »)
                (RawStr, 71..84, «r##" ##"" "##»)
                (Whitespace, 84..89, «
                    »)
                (RawStr, 89..110, «r#"unterminated"
                    »)
            "###]],
        );

        check(r#"r#""#, &expect![[r#"
            (RawStr, 0..3, «r#"»)
        "#]]);
        check(r#"r#"""#, &expect![[r#"
            (RawStr, 0..4, «r#""»)
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
                (Whitespace, 0..5, «
                    »)
                (RawByteStr, 5..11, «br#""#»)
                (Whitespace, 11..16, «
                    »)
                (RawByteStr, 16..24, «br##""##»)
                (Whitespace, 24..29, «
                    »)
                (RawByteStr, 29..52, «br#"raw string""""""""#»)
                (Whitespace, 52..57, «
                    »)
                (RawByteStr, 57..70, «br#"""""""""#»)
                (Whitespace, 70..75, «
                    »)
                (RawByteStr, 75..89, «br##" ##"" "##»)
                (Whitespace, 89..94, «
                    »)
                (RawByteStr, 94..116, «br#"unterminated"
                    »)
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
                (Whitespace, 0..5, «
                    »)
                (RawCStr, 5..11, «cr#""#»)
                (Whitespace, 11..16, «
                    »)
                (RawCStr, 16..24, «cr##""##»)
                (Whitespace, 24..29, «
                    »)
                (RawCStr, 29..52, «cr#"raw string""""""""#»)
                (Whitespace, 52..57, «
                    »)
                (RawCStr, 57..70, «cr#"""""""""#»)
                (Whitespace, 70..75, «
                    »)
                (RawCStr, 75..89, «cr##" ##"" "##»)
                (Whitespace, 89..94, «
                    »)
                (RawCStr, 94..116, «cr#"unterminated"
                    »)
            "###]],
        );
    }
}
