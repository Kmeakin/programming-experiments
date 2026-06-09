use std::debug_assert_matches;
use std::hint::select_unpredictable as select;
use std::simd::Simd;

use crate::TokenKind;
use crate::utils::simdx::*;
use crate::utils::*;

const EOF_BYTE: u8 = 0xff;

pub fn lex_into_soa<const VEC_LEN: usize>(
    padded_src: &[u8],
    kinds: &mut Vec<TokenKind>,
    ends: &mut Vec<u32>,
) {
    lex::<VEC_LEN>(padded_src, |kind, _start, end| unsafe {
        push_unchecked(kinds, kind);
        push_unchecked(ends, end.offset_from_unsigned(padded_src.as_ptr()) as u32);
    });
}

pub fn lex_into_soa_16(padded_src: &[u8], kinds: &mut Vec<TokenKind>, ends: &mut Vec<u32>) {
    lex_into_soa::<16>(padded_src, kinds, ends);
}

pub fn lex_into_soa_32(padded_src: &[u8], kinds: &mut Vec<TokenKind>, ends: &mut Vec<u32>) {
    lex_into_soa::<32>(padded_src, kinds, ends);
}

pub fn lex_into_soa_64(padded_src: &[u8], kinds: &mut Vec<TokenKind>, ends: &mut Vec<u32>) {
    lex_into_soa::<64>(padded_src, kinds, ends);
}

pub fn lex<const VEC_LEN: usize>(
    padded_src: &[u8],
    mut on_token: impl FnMut(TokenKind, *const u8, *const u8),
) {
    let src = padded_src
        .strip_suffix(&[EOF_BYTE; VEC_LEN])
        .expect("Input must be padded with EOF bytes");

    unsafe {
        let mut chunk_ptr = src.as_ptr_range().start;
        let src_end = src.as_ptr_range().end;

        let mut token_start = chunk_ptr;
        let mut carry = Carry::default();
        let mut chunk;

        let lookahead = token_start.cast::<[u8; 4]>().read();
        let mut token_kind = get_kind(lookahead);

        'outer: while chunk_ptr < src_end {
            let vec = load::<VEC_LEN>(chunk_ptr);
            (chunk, carry) = get_chunk(vec, carry);

            while chunk.tokens != 0 {
                match token_kind {
                    TokenKind::LineComment => {
                        chunk.newlines &= chunk.tokens;
                        while chunk.newlines == 0 {
                            chunk_ptr = chunk_ptr.add(VEC_LEN);
                            let vec = load::<VEC_LEN>(chunk_ptr);
                            (chunk, carry) = get_chunk(vec, Carry::default());
                        }
                        {
                            let lowest_one = chunk.newlines.isolate_lowest_one();
                            chunk.tokens &= !(lowest_one.wrapping_sub(1));
                            chunk.tokens |= lowest_one;
                            chunk.newlines &= !lowest_one;
                        }
                        {
                            let tz = (chunk.tokens.trailing_zeros() as usize).min(VEC_LEN);
                            let token_end = chunk_ptr.add(tz);

                            on_token(token_kind, token_start, token_end);

                            token_start = token_end;
                            chunk.tokens &= !chunk.tokens.isolate_lowest_one();

                            let lookahead = token_start.cast::<[u8; 4]>().read();
                            token_kind = get_kind(lookahead);
                        }
                    }
                    TokenKind::BlockComment => {
                        let mut depth = 1u32;
                        let mut token_end = token_start.add(2);
                        loop {
                            match token_end.cast::<[u8; 2]>().read() {
                                [b'/', b'*'] => {
                                    depth += 1;
                                    token_end = token_end.add(2);
                                }
                                [b'*', b'/'] => {
                                    depth -= 1;
                                    token_end = token_end.add(2);
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                [EOF_BYTE, ..] => break,
                                _ => token_end = token_end.add(1),
                            }
                        }
                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk_ptr = token_start;
                        carry = Carry::default();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                        continue 'outer;
                    }
                    TokenKind::Str | TokenKind::ByteStr | TokenKind::CStr => {
                        let mut token_end = token_start.add(1).add(usize::from(matches!(
                            token_kind,
                            TokenKind::ByteStr | TokenKind::CStr
                        )));
                        let token_end = loop {
                            match token_end.read() {
                                b'\\' => token_end = token_end.add(2),
                                b'"' => break token_end.add(1),
                                EOF_BYTE => break token_end,
                                _ => token_end = token_end.add(1),
                            }
                        };

                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk_ptr = token_start;
                        carry = Carry::default();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                        continue 'outer;
                    }
                    TokenKind::Lifetime => {
                        let mut token_end = token_start.add(1);
                        while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = token_end.read()
                        {
                            token_end = token_end.add(1);
                        }
                        (token_end, token_kind) = match token_end.read() {
                            b'\'' => (token_end.add(1), TokenKind::Char),
                            _ => (token_end, TokenKind::Lifetime),
                        };

                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk_ptr = token_start;
                        carry = Carry::default();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                        continue 'outer;
                    }
                    TokenKind::Char | TokenKind::Byte => {
                        let mut token_end = token_start
                            .add(1)
                            .add(usize::from(matches!(token_kind, TokenKind::Byte)));
                        let token_end = loop {
                            match token_end.read() {
                                b'\\' => token_end = token_end.add(2),
                                b'\'' => break token_end.add(1),
                                EOF_BYTE => break token_end,
                                _ => token_end = token_end.add(1),
                            }
                        };

                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk_ptr = token_start;
                        carry = Carry::default();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                        continue 'outer;
                    }
                    TokenKind::RawStr | TokenKind::RawByteStr | TokenKind::RawCStr => {
                        // skip "[b|c]r"
                        chunk.tokens &= !chunk.tokens.isolate_lowest_one();
                        chunk.quotes &= chunk.tokens;
                        while chunk.quotes == 0 {
                            chunk_ptr = chunk_ptr.add(VEC_LEN);
                            let vec = load::<VEC_LEN>(chunk_ptr);
                            (chunk, carry) = get_chunk(vec, Carry::default());
                        }

                        {
                            let lowest_one = chunk.quotes.isolate_lowest_one();
                            chunk.tokens &= !(lowest_one.wrapping_sub(1));
                            chunk.quotes &= !lowest_one;
                            chunk.tokens &= !lowest_one;
                        }

                        if chunk.tokens == 0 {
                            chunk_ptr = chunk_ptr.add(VEC_LEN);
                            let vec = load::<VEC_LEN>(chunk_ptr);
                            (chunk, carry) = get_chunk(vec, carry);
                        }

                        {
                            let tz = (chunk.tokens.trailing_zeros() as usize).min(VEC_LEN);
                            let token_end = chunk_ptr.add(tz);

                            on_token(token_kind, token_start, token_end);

                            token_start = token_end;
                            chunk.tokens &= !chunk.tokens.isolate_lowest_one();

                            let lookahead = token_start.cast::<[u8; 4]>().read();
                            token_kind = get_kind(lookahead);
                        }
                    }
                    TokenKind::HashStr | TokenKind::HashByteStr | TokenKind::HashCStr => {
                        let mut token_end = token_start.add(2).add(usize::from(matches!(
                            token_kind,
                            TokenKind::HashByteStr | TokenKind::HashCStr
                        )));
                        token_kind = match token_kind {
                            TokenKind::HashStr => TokenKind::RawStr,
                            TokenKind::HashByteStr => TokenKind::RawByteStr,
                            TokenKind::HashCStr => TokenKind::RawCStr,
                            _ => token_kind,
                        };
                        let mut hash_count = 1u32;
                        while token_end.read() == b'#' {
                            hash_count += 1;
                            token_end = token_end.add(1);
                        }
                        if token_end.read() == b'"' {
                            token_end = token_end.add(1);
                            token_end = 'raw_str: loop {
                                match token_end.read() {
                                    b'"' => {
                                        token_end = token_end.add(1);
                                        let mut hash_count = hash_count;
                                        while token_end.read() == b'#' {
                                            token_end = token_end.add(1);
                                            hash_count -= 1;
                                            if hash_count == 0 {
                                                break 'raw_str token_end;
                                            }
                                        }
                                    }
                                    EOF_BYTE => break token_end,
                                    _ => token_end = token_end.add(1),
                                }
                            };
                        }

                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk_ptr = token_start;
                        carry = Carry::default();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                        continue 'outer;
                    }
                    TokenKind::RawIdent => {
                        // skip '#'
                        chunk.tokens &= !chunk.tokens.isolate_lowest_one();
                        while chunk.tokens == 0 {
                            chunk_ptr = chunk_ptr.add(VEC_LEN);
                            let vec = load::<VEC_LEN>(chunk_ptr);
                            (chunk, carry) = get_chunk(vec, carry);
                        }

                        // skip to end of ident
                        chunk.tokens &= !chunk.tokens.isolate_lowest_one();
                        while chunk.tokens == 0 {
                            chunk_ptr = chunk_ptr.add(VEC_LEN);
                            let vec = load::<VEC_LEN>(chunk_ptr);
                            (chunk, carry) = get_chunk(vec, carry);
                        }

                        let tz = (chunk.tokens.trailing_zeros() as usize).min(VEC_LEN);
                        let token_end = chunk_ptr.add(tz);

                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk.tokens &= !chunk.tokens.isolate_lowest_one();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                    }
                    TokenKind::Int => {
                        let mut token_end = token_start.add(1);
                        while let b'0'..=b'9' | b'_' = token_end.read() {
                            token_end = token_end.add(1);
                        }
                        match token_end.cast::<[u8; 2]>().read() {
                            [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_'] => {
                                on_token(token_kind, token_start, token_end);
                                token_start = token_end;
                                chunk_ptr = token_start;
                                carry = Carry::default();

                                let lookahead = token_start.cast::<[u8; 4]>().read();
                                token_kind = get_kind(lookahead);
                                continue 'outer;
                            }
                            [b'.', _] => {
                                token_end = token_end.add(1);
                                while let b'0'..=b'9' | b'_' = token_end.read() {
                                    token_end = token_end.add(1);
                                }
                                token_kind = TokenKind::Float;
                            }
                            _ => token_kind = TokenKind::Int,
                        }

                        if let b'e' | b'E' = token_end.read() {
                            token_end = token_end.add(1);
                            token_kind = TokenKind::Float;

                            if let b'+' | b'-' = token_end.read() {
                                token_end = token_end.add(1);
                            }
                        }

                        while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = token_end.read()
                        {
                            token_end = token_end.add(1);
                        }

                        on_token(token_kind, token_start, token_end);
                        token_start = token_end;
                        chunk_ptr = token_start;
                        carry = Carry::default();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                        continue 'outer;
                    }
                    _ => {
                        let tz = (chunk.tokens.trailing_zeros() as usize).min(VEC_LEN);
                        let token_end = chunk_ptr.add(tz);

                        on_token(token_kind, token_start, token_end);

                        token_start = token_end;
                        chunk.tokens &= !chunk.tokens.isolate_lowest_one();

                        let lookahead = token_start.cast::<[u8; 4]>().read();
                        token_kind = get_kind(lookahead);
                    }
                }
            }

            chunk_ptr = chunk_ptr.add(VEC_LEN);
        }
    }
}

#[inline(always)]
fn get_kind(lookahead: [u8; 4]) -> TokenKind {
    match lookahead {
        [b'!', ..] => TokenKind::Bang,
        [b'#', ..] => TokenKind::Hash,
        [b'$', ..] => TokenKind::Dollar,
        [b'%', ..] => TokenKind::Percent,
        [b'&', ..] => TokenKind::Ampersand,
        [b'(', ..] => TokenKind::LParen,
        [b')', ..] => TokenKind::RParen,
        [b'*', ..] => TokenKind::Star,
        [b'+', ..] => TokenKind::Plus,
        [b',', ..] => TokenKind::Comma,
        [b'-', ..] => TokenKind::Minus,
        [b'.', ..] => TokenKind::Dot,
        [b'/', b'/', ..] => TokenKind::LineComment,
        [b'/', b'*', ..] => TokenKind::BlockComment,
        [b'/', ..] => TokenKind::Slash,
        [b':', ..] => TokenKind::Colon,
        [b';', ..] => TokenKind::Semicolon,
        [b'<', ..] => TokenKind::Lt,
        [b'=', ..] => TokenKind::Eq,
        [b'>', ..] => TokenKind::Gt,
        [b'?', ..] => TokenKind::Question,
        [b'[', ..] => TokenKind::LSquare,
        [b']', ..] => TokenKind::RSquare,
        [b'^', ..] => TokenKind::Caret,
        [b'{', ..] => TokenKind::LCurly,
        [b'|', ..] => TokenKind::Bar,
        [b'}', ..] => TokenKind::RCurly,
        [b'~', ..] => TokenKind::Tilde,
        [b'\\', ..] => TokenKind::Backslash,
        [b'`', ..] => TokenKind::Backquote,
        [b'@', ..] => TokenKind::At,

        [b'"', ..] => TokenKind::Str,
        [b'b', b'"', ..] => TokenKind::ByteStr,
        [b'c', b'"', ..] => TokenKind::CStr,

        [b'r', b'"', ..] => TokenKind::RawStr,
        [b'b', b'r', b'"', ..] => TokenKind::RawByteStr,
        [b'c', b'r', b'"', ..] => TokenKind::RawCStr,

        [b'r', b'#', b'"' | b'#', ..] => TokenKind::HashStr,
        [b'b', b'r', b'#', ..] => TokenKind::HashByteStr,
        [b'c', b'r', b'#', ..] => TokenKind::HashCStr,

        [b'r', b'#', ..] => TokenKind::RawIdent,

        [b'\'', b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', ..] => TokenKind::Lifetime,
        [b'\'', ..] => TokenKind::Char,
        [b'b', b'\'', ..] => TokenKind::Byte,

        [b' ' | 0x09..=0x0C, ..] => TokenKind::Whitespace,
        [b'_' | b'a'..=b'z' | b'A'..=b'Z', ..] => TokenKind::Ident,
        [b'0'..=b'9', ..] => TokenKind::Int,
        _ => TokenKind::Unknown,
    }
}

#[derive(Default)]
struct Chunk<const VEC_LEN: usize> {
    newlines: u64,
    quotes:   u64,
    tokens:   u64,
}

/// State that gets carried between chunks.
struct Carry {
    start_of_file: u64,
    whitespace:    u64,
    ident:         u64,
}

impl Default for Carry {
    fn default() -> Self {
        Self {
            start_of_file: 1,
            whitespace:    0,
            ident:         0,
        }
    }
}

fn mask_starts<const VEC_LEN: usize>(mask: u64, carry: &mut u64) -> u64 {
    debug_assert_matches!(*carry, 1 | 0);

    let starts = mask & !(mask << 1) & !*carry;
    *carry = mask >> (VEC_LEN - 1);
    debug_assert_matches!(*carry, 1 | 0);

    starts
}

fn get_chunk<const VEC_LEN: usize>(
    vec: Simd<u8, VEC_LEN>,
    mut carry: Carry,
) -> (Chunk<VEC_LEN>, Carry) {
    let eof_chars = movemask(eq(vec, EOF_BYTE));
    let eof_start = eof_chars.isolate_lowest_one();

    let newlines = movemask(eq(vec, b'\n')) | eof_start;
    let quotes = movemask(eq(vec, b'\"')) | eof_start;
    let ws_chars = movemask(eq(vec, b' ') | in_range(vec, 0x09, 0x0C));
    let id_chars = movemask(
        eq(vec, b'_')
            | in_range(vec, b'a', b'z')
            | in_range(vec, b'A', b'Z')
            | in_range(vec, b'0', b'9'),
    );

    let ws_start = mask_starts::<VEC_LEN>(ws_chars, &mut carry.whitespace);
    let id_start = mask_starts::<VEC_LEN>(id_chars, &mut carry.ident);

    // Any character that is not an alphanumeric char or a whitespace char or an EOF
    // byte is a punctuation char.
    let punct_chars = !ws_chars & !id_chars & !eof_chars;

    let tokens = punct_chars | ws_start | id_start | eof_start;
    let tokens = tokens & !carry.start_of_file;
    let tokens = match VEC_LEN {
        16 => tokens & 0xffff,
        32 => tokens & 0xffff_ffff,
        64 => tokens,
        _ => unreachable!(),
    };

    carry.start_of_file = 0;
    let chunk = Chunk {
        newlines,
        quotes,
        tokens,
    };
    (chunk, carry)
}

#[cfg(test)]
mod tests {
    use std::bstr::ByteStr;

    use expect_test::{Expect, expect};

    use super::*;

    #[track_caller]
    fn check<const VEC_LEN: usize>(src: &str, expect: &Expect) {
        use std::fmt::Write;

        let padded_input: Vec<u8> = src.bytes().chain([EOF_BYTE; VEC_LEN]).collect();
        let mut output = String::new();
        lex::<VEC_LEN>(&padded_input, |kind, start_ptr, end_ptr| unsafe {
            let start_pos = start_ptr.offset_from_unsigned(padded_input.as_ptr());
            let end_pos = end_ptr.offset_from_unsigned(padded_input.as_ptr());
            let lexeme = ByteStr::new(&padded_input[start_pos..end_pos]);
            _ = writeln!(output, "({kind:?}, {start_pos:?}..{end_pos:?}, «{lexeme}»)");
        });
        expect.assert_eq(&output);
    }

    #[test]
    fn empty() {
        check::<16>("", &expect![[r""]]);
        check::<32>("", &expect![[r""]]);
        check::<64>("", &expect![[r""]]);
    }

    #[test]
    fn simple() {
        check::<16>("a", &expect![[r"
            (Ident, 0..1, «a»)
        "]]);
        check::<32>("a", &expect![[r"
            (Ident, 0..1, «a»)
        "]]);
        check::<64>("a", &expect![[r"
            (Ident, 0..1, «a»)
        "]]);

        check::<16>("a bcd efg123", &expect![[r"
            (Ident, 0..1, «a»)
            (Whitespace, 1..2, « »)
            (Ident, 2..5, «bcd»)
            (Whitespace, 5..6, « »)
            (Ident, 6..12, «efg123»)
        "]]);

        check::<32>("a bcd efg123", &expect![[r"
            (Ident, 0..1, «a»)
            (Whitespace, 1..2, « »)
            (Ident, 2..5, «bcd»)
            (Whitespace, 5..6, « »)
            (Ident, 6..12, «efg123»)
        "]]);

        check::<64>("a bcd efg123", &expect![[r"
            (Ident, 0..1, «a»)
            (Whitespace, 1..2, « »)
            (Ident, 2..5, «bcd»)
            (Whitespace, 5..6, « »)
            (Ident, 6..12, «efg123»)
        "]]);
    }

    #[test]
    fn punct() {
        check::<16>(r"!#$%&()*+,-./:;<=>?[]^{|}~\`", &expect![[r"
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
            (Backslash, 26..27, «\»)
            (Backquote, 27..28, «`»)
        "]]);

        check::<32>(r"!#$%&()*+,-./:;<=>?[]^{|}~\`", &expect![[r"
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
            (Backslash, 26..27, «\»)
            (Backquote, 27..28, «`»)
        "]]);

        check::<64>(r"!#$%&()*+,-./:;<=>?[]^{|}~\`", &expect![[r"
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
            (Backslash, 26..27, «\»)
            (Backquote, 27..28, «`»)
        "]]);
    }

    /// Line comments where the newline/EOF is in the same chunk as the `//`.
    #[test]
    fn line_comments_easy() {
        check::<16>("//hello\n", &expect![[r"
            (LineComment, 0..7, «//hello»)
            (Whitespace, 7..8, «
            »)
        "]]);

        check::<16>("//hello\nworld", &expect![[r"
            (LineComment, 0..7, «//hello»)
            (Whitespace, 7..8, «
            »)
            (Ident, 8..13, «world»)
        "]]);

        check::<16>("//hello\n//bye", &expect![[r"
            (LineComment, 0..7, «//hello»)
            (Whitespace, 7..8, «
            »)
            (LineComment, 8..13, «//bye»)
        "]]);

        check::<16>("//a\n//b\n//c\n//d\n//e", &expect![[r"
            (LineComment, 0..3, «//a»)
            (Whitespace, 3..4, «
            »)
            (LineComment, 4..7, «//b»)
            (Whitespace, 7..8, «
            »)
            (LineComment, 8..11, «//c»)
            (Whitespace, 11..12, «
            »)
            (LineComment, 12..15, «//d»)
            (Whitespace, 15..16, «
            »)
            (LineComment, 16..19, «//e»)
        "]]);
    }

    /// Line comments where the newline/EOF is not in the same chunk as the
    /// `//`.
    #[test]
    fn line_comments_hard() {
        check::<16>("//23456789abcdefghijklmn\n", &expect![[r"
            (LineComment, 0..24, «//23456789abcdefghijklmn»)
            (Whitespace, 24..25, «
            »)
        "]]);

        check::<16>("//23456789abcdefghijklmn\n//foo\nhello", &expect![[r"
            (LineComment, 0..24, «//23456789abcdefghijklmn»)
            (Whitespace, 24..25, «
            »)
            (LineComment, 25..30, «//foo»)
            (Whitespace, 30..31, «
            »)
            (Ident, 31..36, «hello»)
        "]]);
    }

    #[test]
    fn block_comments() {
        check::<16>("/**/", &expect![[r"
            (BlockComment, 0..4, «/**/»)
        "]]);
        check::<16>("/*foo*/", &expect![[r"
            (BlockComment, 0..7, «/*foo*/»)
        "]]);
        check::<16>("/*foo*/", &expect![[r"
            (BlockComment, 0..7, «/*foo*/»)
        "]]);
    }

    #[test]
    fn block_comments_unterminated() {
        check::<16>("/* EOF", &expect![[r"
            (BlockComment, 0..6, «/* EOF»)
        "]]);
        check::<16>("/*/ EOF", &expect![[r"
            (BlockComment, 0..7, «/*/ EOF»)
        "]]);
        check::<16>("/*/*/ EOF", &expect![[r"
            (BlockComment, 0..9, «/*/*/ EOF»)
        "]]);
        check::<16>("/*/**/ EOF", &expect![[r"
            (BlockComment, 0..10, «/*/**/ EOF»)
        "]]);
        check::<16>("/*/**/*/ EOF", &expect![[r"
            (BlockComment, 0..8, «/*/**/*/»)
            (Whitespace, 8..9, « »)
            (Ident, 9..12, «EOF»)
        "]]);
    }
}
