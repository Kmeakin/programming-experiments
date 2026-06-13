#![allow(unsafe_op_in_unsafe_fn)]

use crate::common::{EOF_BYTE, Lexer, TokenKind};
use crate::utils::memchr_raw;

/// Only exported for `cargo asm`. Don't actually call!
pub fn lex_soa(padded_src: &[u8], kinds: &mut Vec<TokenKind>, ends: &mut Vec<u32>) {
    let kinds_ptr = kinds.as_mut_ptr();
    let ends_ptr = ends.as_mut_ptr();
    lex(
        padded_src,
        (kinds_ptr, ends_ptr),
        |(kinds_ptr, ends_ptr), kind, _start, end| unsafe {
            kinds_ptr.write(kind);
            ends_ptr.write(end as u32);
            (kinds_ptr.add(1), ends_ptr.add(1))
        },
    );
}

pub fn lex<B>(
    padded_src: &[u8],
    mut acc: B,
    mut on_token: impl FnMut(B, TokenKind, *const u8, *const u8) -> B,
) -> B {
    unsafe {
        if cfg!(debug_assertions) {
            padded_src
                .strip_suffix(&[EOF_BYTE; 16])
                .expect("Input should be padded with EOF_BYTE");
        }

        let src_start = padded_src.as_ptr();
        let src_end = padded_src.as_ptr_range().end.sub(16);
        let mut token_start = src_start;
        let mut state = lookup_state(token_start.read());

        #[loop_match]
        loop {
            state = 'state: {
                macro_rules! next_state {
                    ($b:expr) => {
                        match lookup_state($b) {
                            State::Whitespace => {
                                #[const_continue]
                                break 'state State::Whitespace;
                            }
                            State::Alpha => {
                                #[const_continue]
                                break 'state State::Alpha;
                            }
                            State::Digit => {
                                #[const_continue]
                                break 'state State::Digit;
                            }
                            State::B => {
                                #[const_continue]
                                break 'state State::B;
                            }
                            State::C => {
                                #[const_continue]
                                break 'state State::C;
                            }
                            State::R => {
                                #[const_continue]
                                break 'state State::R;
                            }
                            State::Punct => {
                                #[const_continue]
                                break 'state State::Punct;
                            }
                            State::Slash => {
                                #[const_continue]
                                break 'state State::Slash;
                            }
                            State::Apostrophe => {
                                #[const_continue]
                                break 'state State::Apostrophe;
                            }
                            State::Quote => {
                                #[const_continue]
                                break 'state State::Quote;
                            }
                            State::Unknown => {
                                #[const_continue]
                                break 'state State::Unknown;
                            }
                            State::Eof => {
                                #[const_continue]
                                break 'state State::Eof;
                            }
                        }
                    };
                }

                match state {
                    State::Whitespace => {
                        let token_end = eat_whitespace(token_start.add(1));
                        acc = on_token(acc, TokenKind::Whitespace, token_start, token_end);
                        token_start = token_end;
                        next_state!(token_start.read());
                    }
                    State::Alpha => {
                        let token_end = eat_ident_cont(token_start.add(1));
                        acc = on_token(acc, TokenKind::Ident, token_start, token_end);
                        token_start = token_end;
                        next_state!(token_start.read());
                    }
                    State::Digit => {
                        let mut token_end = token_start.add(1);
                        token_end = eat_digits(token_end);
                        let mut kind = match token_end.cast::<[u8; 2]>().read() {
                            [b'.', b'.', ..] => {
                                acc = on_token(acc, TokenKind::Int, token_start, token_end);
                                token_start = token_end;
                                next_state!(token_start.read());
                            }
                            [b'.', b, ..] if is_ident_start(b) => {
                                acc = on_token(acc, TokenKind::Int, token_start, token_end);
                                token_start = token_end;
                                next_state!(token_start.read());
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
                        acc = on_token(acc, kind, token_start, token_end);
                        token_start = token_end;
                        next_state!(token_start.read());
                    }
                    State::Punct => {
                        while let Some(kind) = is_punct(token_start.read()) {
                            acc = on_token(acc, kind, token_start, token_start.add(1));
                            token_start = token_start.add(1);
                        }
                        next_state!(token_start.read());
                    }
                    State::Slash => match token_start.add(1).read() {
                        b'/' => {
                            let token_end =
                                memchr_raw(b'\n', token_start, src_end).unwrap_or(src_end);
                            acc = on_token(acc, TokenKind::LineComment, token_start, token_end);
                            token_start = token_end;
                            next_state!(token_start.read());
                        }
                        b'*' => {
                            let token_end = block_comment(token_start, src_end);
                            acc = on_token(acc, TokenKind::BlockComment, token_start, token_end);
                            token_start = token_end;
                            next_state!(token_start.read());
                        }
                        _ => {
                            let end = token_start.add(1);
                            acc = on_token(acc, TokenKind::Slash, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                    },
                    State::B => match token_start.cast::<[u8; 4]>().read() {
                        [b'b', b'\'', ..] => {
                            let end = single_quote_string(token_start.add(1));
                            acc = on_token(acc, TokenKind::BChar, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'b', b'"', ..] => {
                            let end = double_quote_string(token_start.add(1), src_end);
                            acc = on_token(acc, TokenKind::BStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'b', b'r', b'"', ..] => {
                            let end = raw_string(token_start.add(1), src_end);
                            acc = on_token(acc, TokenKind::RawBStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'b', b'r', b'#', ..] => {
                            let end = raw_hash_string(token_start.add(1), src_end);
                            acc = on_token(acc, TokenKind::RawBStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        _ => {
                            let end = eat_ident_cont(token_start.add(1));
                            acc = on_token(acc, TokenKind::Ident, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                    },
                    State::C => match token_start.cast::<[u8; 4]>().read() {
                        [b'c', b'"', ..] => {
                            let end = double_quote_string(token_start.add(1), src_end);
                            acc = on_token(acc, TokenKind::CStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'c', b'r', b'"', ..] => {
                            let end = raw_string(token_start.add(1), src_end);
                            acc = on_token(acc, TokenKind::RawCStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'c', b'r', b'#', ..] => {
                            let end = raw_hash_string(token_start.add(1), src_end);
                            acc = on_token(acc, TokenKind::RawCStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        _ => {
                            let end = eat_ident_cont(token_start.add(1));
                            acc = on_token(acc, TokenKind::Ident, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                    },
                    State::R => match token_start.cast::<[u8; 4]>().read() {
                        [b'r', b'"', ..] => {
                            let end = raw_string(token_start, src_end);
                            acc = on_token(acc, TokenKind::RawStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'r', b'#', b'#' | b'"', ..] => {
                            let end = raw_hash_string(token_start, src_end);
                            acc = on_token(acc, TokenKind::RawStr, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                        [b'r', b'#', ..] => {
                            let token_end = eat_ident_cont(token_start.add(2));
                            acc = on_token(acc, TokenKind::RawIdent, token_start, token_end);
                            token_start = token_end;
                            next_state!(token_start.read());
                        }
                        _ => {
                            let end = eat_ident_cont(token_start.add(1));
                            acc = on_token(acc, TokenKind::Ident, token_start, end);
                            token_start = end;
                            next_state!(token_start.read());
                        }
                    },
                    State::Apostrophe => {
                        let mut end = token_start.add(1);
                        if is_ident_start(end.read()) {
                            while is_ident_cont(end.read()) {
                                end = end.add(1);
                            }
                            match end.read() {
                                b'\'' => {
                                    end = end.add(1);
                                    acc = on_token(acc, TokenKind::Char, token_start, end);
                                    token_start = end;
                                    next_state!(token_start.read());
                                }
                                _ => {
                                    acc = on_token(acc, TokenKind::Lifetime, token_start, end);
                                    token_start = end;
                                    next_state!(token_start.read());
                                }
                            }
                        }

                        end = single_quote_string(token_start);
                        acc = on_token(acc, TokenKind::Char, token_start, end);
                        token_start = end;
                        next_state!(token_start.read());
                    }
                    State::Quote => {
                        let end = double_quote_string(token_start, src_end);
                        acc = on_token(acc, TokenKind::Str, token_start, end);
                        token_start = end;
                        next_state!(token_start.read());
                    }
                    State::Unknown => {
                        acc = on_token(acc, TokenKind::Unknown, token_start, token_start.add(1));
                        token_start = token_start.add(1);
                        next_state!(token_start.read());
                    }
                    State::Eof => return acc,
                }
            };
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum State {
    Whitespace,
    Alpha,
    Digit,

    B,
    C,
    R,

    Punct,
    Slash,
    Apostrophe,
    Quote,

    Unknown,
    Eof,
}

static STATE_LUT: [State; 256] = {
    let mut lut = [State::Unknown; 256];
    let mut i = 0;
    while i < 256 {
        lut[i] = match i as u8 {
            b' ' | 0x09..=0x0C => State::Whitespace,
            b'b' => State::B,
            b'c' => State::C,
            b'r' => State::R,
            b'_' | b'a'..=b'z' | b'A'..=b'Z' => State::Alpha,
            b'0'..=b'9' => State::Digit,
            b'\'' => State::Apostrophe,
            b'\"' => State::Quote,
            b'/' => State::Slash,
            | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-' | b'*'
            | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.' | b'!'
            | b'>' | b'<' | b'^' => State::Punct,
            EOF_BYTE => State::Eof,
            _ => State::Unknown,
        };
        i += 1;
    }
    lut
};
const fn lookup_state(b: u8) -> State { STATE_LUT[b as usize] }

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
const unsafe fn eat_whitespace(mut cursor: *const u8) -> *const u8 {
    loop {
        let array = cursor.cast::<[u8; 8]>().read();
        if !is_whitespace(array[0]) {
            return cursor.add(0);
        }
        if !is_whitespace(array[1]) {
            return cursor.add(1);
        }
        if !is_whitespace(array[2]) {
            return cursor.add(2);
        }
        if !is_whitespace(array[3]) {
            return cursor.add(3);
        }
        if !is_whitespace(array[4]) {
            return cursor.add(4);
        }
        if !is_whitespace(array[5]) {
            return cursor.add(5);
        }
        if !is_whitespace(array[6]) {
            return cursor.add(6);
        }
        if !is_whitespace(array[7]) {
            return cursor.add(7);
        }
        cursor = cursor.add(8);
    }
}

#[inline]
const unsafe fn eat_digits(mut cursor: *const u8) -> *const u8 {
    while is_digit(cursor.read()) {
        cursor = cursor.add(1);
    }
    cursor
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

#[inline]
unsafe fn block_comment(start: *const u8, _src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"/*");
    let mut cursor = start.add(2);
    let mut depth = 1usize;
    loop {
        match cursor.cast::<[u8; 2]>().read() {
            [b'/', b'*', ..] => {
                cursor = cursor.add(2);
                depth += 1;
            }
            [b'*', b'/', ..] => {
                cursor = cursor.add(2);
                depth -= 1;
                if depth == 0 {
                    return cursor;
                }
            }
            [EOF_BYTE, ..] => return cursor,
            _ => cursor = cursor.add(1),
        }
    }
}

#[inline]
#[cfg(false)]
unsafe fn block_comment(start: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"/*");

    let mut depth = 1usize;
    let haystack = std::slice::from_ptr_range(start.add(2)..src_end);
    for pos in memchr::memchr2_iter(b'/', b'*', haystack) {
        let mut cursor = haystack.as_ptr().add(pos);
        debug_assert_matches!(cursor.read(), b'/' | b'*');
        match cursor.cast::<[u8; 2]>().read() {
            [b'/', b'*', ..] => {
                cursor = cursor.add(2);
                depth += 1;
            }
            [b'*', b'/', ..] => {
                cursor = cursor.add(2);
                depth -= 1;
                if depth == 0 {
                    return cursor;
                }
            }
            _ => cursor = cursor.add(1),
        }
    }
    src_end
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

#[derive(Copy, Clone)]
pub struct ScalarLoopMatch {}
impl Lexer for ScalarLoopMatch {
    fn lex_bytes<B>(
        &self,
        bytes: &[u8],
        acc: B,
        on_token: impl FnMut(B, TokenKind, *const u8, *const u8) -> B,
    ) -> B {
        lex(bytes, acc, on_token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integration() { crate::tests::integration_tests(ScalarLoopMatch {}); }
}
