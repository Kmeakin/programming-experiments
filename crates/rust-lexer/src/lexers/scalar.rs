//! Changes WRT to `rustc`:
//! * Iterates over bytes instead of Unicode chars
//! * Eliminate bounds checks by padding the input with `EOF_BYTE` (0xFF, cannot
//!   occur in valid UTF8).
//! * Use a LUT for character classification instead of branches.
//! * Use `memchr` for finding the end of line comments and strings.
//! * FIXME: handle Unicode whitespace and identifiers

#![allow(unsafe_op_in_unsafe_fn)]

use std::time::Duration;

use crate::common::{EOF_BYTE, Lexer, TokenKind};
use crate::utils::memchr_raw;

/// Only exported for `cargo asm`. Don't actually call!
pub fn lex_soa(padded_src: &[u8], kinds: &mut Vec<TokenKind>, ends: &mut Vec<u32>) {
    let mut kinds_ptr = kinds.as_mut_ptr();
    let mut ends_ptr = ends.as_mut_ptr();
    lex(padded_src, |kind, _start, end| unsafe {
        kinds_ptr.write(kind);
        kinds_ptr = kinds_ptr.add(1);
        ends_ptr.write(end as u32);
        ends_ptr = ends_ptr.add(1);
    });
}

#[rustfmt::skip]
const LUT: [u8; 256] = {
    let mut lut = [0; 256];
    let mut i = 0;
    while i < 256 {
        // whitespace
        lut[i] |= (matches!(i as u8, | b' ' | 0x09..=0x0C) as u8);

        // digits
        lut[i] |= (matches!(i as u8, | b'_' | b'0'..=b'9') as u8) << 1;

        // ident starts
        lut[i] |= (matches!(i as u8, | b'_' | b'a'..=b'z' | b'A'..=b'Z') as u8) << 2;

        // ident conts
        lut[i] |= (matches!(i as u8, | b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') as u8) << 3;

        // punctuation
        lut[i] |= (matches!(i as u8, | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';'
                                     | b':' | b'+' | b'-' | b'*' | b'%' | b'=' | b'&' | b'|'
                                     | b'$' | b'?' | b'~' | b'#' | b'@' | b'.' | b'!' | b'>'
                                     | b'<' | b'^') as u8) << 4;
        i += 1;
    }
    lut
};

#[inline]
const fn is_whitespace(byte: u8) -> bool { LUT[byte as usize] & (1 << 0) != 0 }

#[inline]
const fn is_digit(byte: u8) -> bool { LUT[byte as usize] & (1 << 1) != 0 }

#[inline]
const fn is_ident_start(byte: u8) -> bool { LUT[byte as usize] & (1 << 2) != 0 }

#[inline]
const fn is_ident_cont(byte: u8) -> bool { LUT[byte as usize] & (1 << 3) != 0 }

#[inline]
const fn is_punct(b: u8) -> Option<TokenKind> {
    if LUT[b as usize] & (1 << 4) != 0 {
        unsafe { Some(std::mem::transmute::<u8, TokenKind>(b)) }
    } else {
        None
    }
}

#[inline]
const unsafe fn eat_ident_cont(mut cursor: *const u8) -> *const u8 {
    loop {
        let array = cursor.cast::<[u8; 8]>().read();
        if !is_ident_cont(array[0]) {
            return cursor.add(0);
        }
        if !is_ident_cont(array[1]) {
            return cursor.add(1);
        }
        if !is_ident_cont(array[2]) {
            return cursor.add(2);
        }
        if !is_ident_cont(array[3]) {
            return cursor.add(3);
        }
        if !is_ident_cont(array[4]) {
            return cursor.add(4);
        }
        if !is_ident_cont(array[5]) {
            return cursor.add(5);
        }
        if !is_ident_cont(array[6]) {
            return cursor.add(6);
        }
        if !is_ident_cont(array[7]) {
            return cursor.add(7);
        }
        cursor = cursor.add(8);
    }
}

pub fn lex(padded_src: &[u8], mut on_token: impl FnMut(TokenKind, *const u8, *const u8)) {
    unsafe {
        if cfg!(debug_assertions) {
            padded_src
                .strip_suffix(&[EOF_BYTE; 16])
                .expect("Input should be padded with EOF_BYTE");
        }

        let src_start = padded_src.as_ptr();
        let src_end = padded_src.as_ptr_range().end.sub(16);
        let mut token_start = src_start;

        loop {
            match token_start.cast::<[u8; 4]>().read() {
                [b'/', b'/', ..] => {
                    let token_end = memchr_raw(b'\n', token_start, src_end).unwrap_or(src_end);
                    on_token(TokenKind::LineComment, token_start, token_end);
                    token_start = token_end;
                }
                [b'/', b'*', ..] => {
                    let mut end = token_start.add(2);
                    let mut depth = 1usize;
                    end = loop {
                        match end.cast::<[u8; 2]>().read() {
                            [b'/', b'*', ..] => {
                                end = end.add(2);
                                depth += 1;
                            }
                            [b'*', b'/', ..] => {
                                end = end.add(2);
                                depth -= 1;
                                if depth == 0 {
                                    break end;
                                }
                            }
                            [EOF_BYTE, ..] => break end,
                            _ => end = end.add(1),
                        }
                    };
                    on_token(TokenKind::BlockComment, token_start, end);
                    token_start = end;
                }
                #[rustfmt::skip]
                [b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-'
                      | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@'
                      | b'.' | b'!' | b'>' | b'<' | b'^', ..] => {
                    while let Some(kind) = is_punct(token_start.read()) {
                        on_token(kind, token_start, token_start.add(1));
                        token_start = token_start.add(1);
                    };
                }
                [b'/', ..] => {
                    let token_end = token_start.add(1);
                    on_token(TokenKind::Slash, token_start, token_end);
                    token_start = token_end;
                }

                [b'"', ..] => {
                    let end = double_quote_string(token_start, src_end);
                    on_token(TokenKind::Str, token_start, end);
                    token_start = end;
                }
                [b'\'', ..] => {
                    let mut end = token_start.add(1);
                    if is_ident_start(end.read()) {
                        while is_ident_cont(end.read()) {
                            end = end.add(1);
                        }
                        match end.read() {
                            b'\'' => {
                                end = end.add(1);
                                on_token(TokenKind::Char, token_start, end);
                                token_start = end;
                            }
                            _ => {
                                on_token(TokenKind::Lifetime, token_start, end);
                                token_start = end;
                            }
                        }
                    } else {
                        end = single_quote_string(token_start);
                        on_token(TokenKind::Char, token_start, end);
                        token_start = end;
                    }
                }

                [b'b', b'\'', ..] => {
                    let end = single_quote_string(token_start.add(1));
                    on_token(TokenKind::BChar, token_start, end);
                    token_start = end;
                }
                [b'b', b'"', ..] => {
                    let end = double_quote_string(token_start.add(1), src_end);
                    on_token(TokenKind::BStr, token_start, end);
                    token_start = end;
                }
                [b'b', b'r', b'"', ..] => {
                    let end = raw_string(token_start.add(1), src_end);
                    on_token(TokenKind::RawBStr, token_start, end);
                    token_start = end;
                }
                [b'b', b'r', b'#', ..] => {
                    let end = raw_hash_string(token_start.add(1), src_end);
                    on_token(TokenKind::RawBStr, token_start, end);
                    token_start = end;
                }

                [b'c', b'"', ..] => {
                    let end = double_quote_string(token_start.add(1), src_end);
                    on_token(TokenKind::CStr, token_start, end);
                    token_start = end;
                }
                [b'c', b'r', b'"', ..] => {
                    let end = raw_string(token_start.add(1), src_end);
                    on_token(TokenKind::RawCStr, token_start, end);
                    token_start = end;
                }
                [b'c', b'r', b'#', ..] => {
                    let end = raw_hash_string(token_start.add(1), src_end);
                    on_token(TokenKind::RawCStr, token_start, end);
                    token_start = end;
                }

                [b'r', b'"', ..] => {
                    let end = raw_string(token_start, src_end);
                    on_token(TokenKind::RawStr, token_start, end);
                    token_start = end;
                }
                [b'r', b'#', b'#' | b'"', ..] => {
                    let end = raw_hash_string(token_start, src_end);
                    on_token(TokenKind::RawStr, token_start, end);
                    token_start = end;
                }
                [b'r', b'#', ..] => {
                    let mut token_end = token_start.add(2);
                    token_end = eat_ident_cont(token_end);
                    on_token(TokenKind::RawIdent, token_start, token_end);
                    token_start = token_end;
                }

                [b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                    let token_end = eat_ident_cont(token_start.add(1));
                    on_token(TokenKind::Ident, token_start, token_end);
                    token_start = token_end;
                }
                [b'0'..=b'9', ..] => {
                    let mut token_end = token_start.add(1);
                    while is_digit(token_end.read()) {
                        token_end = token_end.add(1);
                    }
                    let mut kind = match token_end.cast::<[u8; 2]>().read() {
                        [b'.', b'.', ..] => {
                            on_token(TokenKind::Int, token_start, token_end);
                            token_start = token_end;
                            continue;
                        }
                        [b'.', b, ..] if is_ident_start(b) => {
                            on_token(TokenKind::Int, token_start, token_end);
                            token_start = token_end;
                            continue;
                        }
                        [b'.', ..] => {
                            token_end = token_end.add(1);
                            while is_digit(token_end.read()) {
                                token_end = token_end.add(1);
                            }
                            TokenKind::Float
                        }
                        _ => TokenKind::Int,
                    };

                    if let b'e' | b'E' = token_end.read() {
                        kind = TokenKind::Float;

                        token_end = token_end.add(1);
                        if let b'+' | b'-' = token_end.read() {
                            token_end = token_end.add(1);
                        }
                    }

                    token_end = eat_ident_cont(token_end);
                    on_token(kind, token_start, token_end);
                    token_start = token_end;
                }

                [b' ' | 0x09..=0x0C, ..] => {
                    let mut token_end = token_start.add(1);
                    while is_whitespace(token_end.read()) {
                        token_end = token_end.add(1);
                    }
                    on_token(TokenKind::Whitespace, token_start, token_end);
                    token_start = token_end;
                }

                [EOF_BYTE, ..] => return,

                _ => {
                    let end = token_start.add(1);
                    on_token(TokenKind::Unknown, token_start, end);
                    token_start = end;
                }
            }
        }
    }
}

#[inline]
unsafe fn single_quote_string(start: *const u8) -> *const u8 {
    debug_assert_eq!(start.read(), b'\'');
    let mut end = start.add(1);
    loop {
        match end.read() {
            b'\\' => end = end.add(2),
            b'\'' => return end.add(1),
            EOF_BYTE => return end,
            _ => end = end.add(1),
        }
    }
}

#[inline]
unsafe fn double_quote_string(mut cursor: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(cursor.read(), b'\"');
    cursor = cursor.add(1);

    let haystack = std::slice::from_ptr_range(cursor..src_end);
    for pos in memchr::memchr_iter(b'"', haystack) {
        let quote = haystack.as_ptr().add(pos);
        debug_assert_eq!(quote.read(), b'\"');

        let after_quote = quote.add(1);

        let mut num_backslashes = 0usize;
        let mut backslash_ptr = quote.sub(1);
        while backslash_ptr.read() == b'\\' {
            backslash_ptr = backslash_ptr.sub(1);
            num_backslashes += 1;
        }
        if num_backslashes.is_multiple_of(2) {
            return after_quote;
        }
    }
    src_end
}

#[inline]
unsafe fn raw_string(start: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"r\"");
    match memchr_raw(b'"', start.add(2), src_end) {
        Some(end) => end.add(1),
        None => src_end,
    }
}

#[inline]
unsafe fn raw_hash_string(cursor: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(cursor.cast::<[u8; 2]>().read(), *b"r#");
    let mut cursor = cursor.add(2);
    let mut num_hashes = 1usize;
    while cursor.read() == b'#' {
        cursor = cursor.add(1);
        num_hashes += 1;
    }

    if cursor.read() != b'\"' {
        return cursor;
    }
    cursor = cursor.add(1);

    let haystack = std::slice::from_ptr_range(cursor..src_end);
    for pos in memchr::memchr_iter(b'"', haystack) {
        cursor = haystack.as_ptr().add(pos);
        cursor = cursor.add(1);
        let mut num_hashes = num_hashes;
        while cursor.read() == b'#' {
            cursor = cursor.add(1);
            num_hashes -= 1;
            if num_hashes == 0 {
                return cursor;
            }
        }
    }
    src_end
}

pub struct Scalar {}
impl Lexer for Scalar {
    fn lex_bytes(
        &self,
        bytes: &[u8],
        mut on_token: impl FnMut(TokenKind, *const u8, *const u8),
    ) -> Duration {
        let start = std::time::Instant::now();
        lex(bytes, |kind, start, end| {
            on_token(kind, start, end);
        });
        start.elapsed()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integration() { crate::tests::integration_tests(Scalar {}); }
}
