#![allow(unsafe_op_in_unsafe_fn)]

use std::debug_assert_matches;

use crate::common::{EOF_BYTE, Lexer, TokenKind};
use crate::utils::*;

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
    acc: B,
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
        let lex_state = const { &LexState::new() };
        lex_state.fns[src_start.read() as usize](lex_state, acc, &mut on_token, src_start, src_end)
    }
}

type LexFn<B, F> = unsafe fn(&LexState<B, F>, B, F, *const u8, *const u8) -> B;

struct LexState<B, F: FnMut(B, TokenKind, *const u8, *const u8) -> B> {
    fns: [LexFn<B, F>; 256],
}

impl<B, F: FnMut(B, TokenKind, *const u8, *const u8) -> B> LexState<B, F> {
    pub const fn new() -> Self {
        let fns = const {
            let mut fns: [LexFn<B, F>; 256] = [Self::unknown; 256];
            let mut i = 0;
            while i < 256 {
                fns[i] = match i as u8 {
                    b' ' | 0x09..=0x0C => Self::whitespace,
                    b'b' => Self::b,
                    b'c' => Self::c,
                    b'r' => Self::r,
                    b'_' | b'a'..=b'z' | b'A'..=b'Z' => Self::alpha,
                    b'0'..=b'9' => Self::digit,
                    b'\'' => Self::apostrophe,
                    b'\"' => Self::quote,
                    b'/' => Self::slash,
                    | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+'
                    | b'-' | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#'
                    | b'@' | b'.' | b'!' | b'>' | b'<' | b'^' => Self::punct,
                    EOF_BYTE => Self::eof,
                    _ => Self::unknown,
                };
                i += 1;
            }
            fns
        };
        Self { fns }
    }

    fn next_state(&self, byte: u8) -> LexFn<B, F> { self.fns[byte as usize] }

    unsafe fn eof(
        _: &Self,
        acc: B,
        _on_token: F,
        _token_start: *const u8,
        _src_end: *const u8,
    ) -> B {
        acc
    }

    unsafe fn unknown(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        let token_end = token_start.add(1);
        let acc = on_token(acc, TokenKind::Unknown, token_start, token_end);
        become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
    }

    unsafe fn whitespace(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        let token_end = eat_whitespace(token_start.add(1));
        let acc = on_token(acc, TokenKind::Whitespace, token_start, token_end);
        become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
    }

    unsafe fn punct(
        &self,
        mut acc: B,
        mut on_token: F,
        mut token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        debug_assert_matches!(is_punct(token_start.read()), Some(_));

        while let Some(kind) = is_punct(token_start.read()) {
            acc = on_token(acc, kind, token_start, token_start.add(1));
            token_start = token_start.add(1);
        }
        become self.next_state(token_start.read())(self, acc, on_token, token_start, src_end)
    }

    unsafe fn slash(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        debug_assert_eq!(token_start.read(), b'/');
        match token_start.add(1).read() {
            b'/' => {
                let token_end = line_comment(token_start, src_end);
                let acc = on_token(acc, TokenKind::LineComment, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            b'*' => {
                let token_end = block_comment(token_start, src_end);
                let acc = on_token(acc, TokenKind::BlockComment, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            _ => {
                let token_end = token_start.add(1);
                let acc = on_token(acc, TokenKind::Slash, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
        }
    }

    unsafe fn alpha(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        debug_assert!(is_ident_start(token_start.read()));
        let token_end = eat_ident_cont(token_start.add(1));
        let acc = on_token(acc, TokenKind::Ident, token_start, token_end);
        become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
    }

    unsafe fn digit(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        let mut token_end = token_start.add(1);
        token_end = eat_digits(token_end);
        let mut kind = match token_end.cast::<[u8; 2]>().read() {
            [b'.', b'.', ..] => {
                let acc = on_token(acc, TokenKind::Int, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'.', b, ..] if is_ident_start(b) => {
                let acc = on_token(acc, TokenKind::Int, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
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

        let token_end = eat_ident_cont(token_end);
        let acc = on_token(acc, kind, token_start, token_end);
        become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
    }

    unsafe fn quote(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        let token_end = double_quote_string(token_start, src_end);
        let acc = on_token(acc, TokenKind::Str, token_start, token_end);
        become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
    }

    unsafe fn apostrophe(
        &self,
        acc: B,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) -> B {
        let mut token_end = token_start.add(1);
        if is_ident_start(token_end.read()) {
            while is_ident_cont(token_end.read()) {
                token_end = token_end.add(1);
            }
            match token_end.read() {
                b'\'' => {
                    token_end = token_end.add(1);
                    let acc = on_token(acc, TokenKind::Char, token_start, token_end);
                    become self.next_state(token_end.read())(
                        self, acc, on_token, token_end, src_end,
                    )
                }
                _ => {
                    let acc = on_token(acc, TokenKind::Lifetime, token_start, token_end);
                    become self.next_state(token_end.read())(
                        self, acc, on_token, token_end, src_end,
                    )
                }
            }
        }

        token_end = single_quote_string(token_start);
        let acc = on_token(acc, TokenKind::Char, token_start, token_end);
        become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
    }

    unsafe fn b(&self, acc: B, mut on_token: F, token_start: *const u8, src_end: *const u8) -> B {
        match token_start.cast::<[u8; 4]>().read() {
            [b'b', b'\'', ..] => {
                let token_end = single_quote_string(token_start.add(1));
                let acc = on_token(acc, TokenKind::BChar, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'b', b'"', ..] => {
                let token_end = double_quote_string(token_start.add(1), src_end);
                let acc = on_token(acc, TokenKind::BStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'b', b'r', b'"', ..] => {
                let token_end = raw_string(token_start.add(1), src_end);
                let acc = on_token(acc, TokenKind::RawBStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'b', b'r', b'#', ..] => {
                let token_end = raw_hash_string(token_start.add(1), src_end);
                let acc = on_token(acc, TokenKind::RawBStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            _ => {
                let token_end = eat_ident_cont(token_start.add(1));
                let acc = on_token(acc, TokenKind::Ident, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
        }
    }
    unsafe fn c(&self, acc: B, mut on_token: F, token_start: *const u8, src_end: *const u8) -> B {
        match token_start.cast::<[u8; 4]>().read() {
            [b'c', b'"', ..] => {
                let token_end = double_quote_string(token_start.add(1), src_end);
                let acc = on_token(acc, TokenKind::CStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'c', b'r', b'"', ..] => {
                let token_end = raw_string(token_start.add(1), src_end);
                let acc = on_token(acc, TokenKind::RawCStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'c', b'r', b'#', ..] => {
                let token_end = raw_hash_string(token_start.add(1), src_end);
                let acc = on_token(acc, TokenKind::RawCStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            _ => {
                let token_end = eat_ident_cont(token_start.add(1));
                let acc = on_token(acc, TokenKind::Ident, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
        }
    }
    unsafe fn r(&self, acc: B, mut on_token: F, token_start: *const u8, src_end: *const u8) -> B {
        match token_start.cast::<[u8; 4]>().read() {
            [b'r', b'"', ..] => {
                let token_end = raw_string(token_start, src_end);
                let acc = on_token(acc, TokenKind::RawStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'r', b'#', b'#' | b'"', ..] => {
                let token_end = raw_hash_string(token_start, src_end);
                let acc = on_token(acc, TokenKind::RawStr, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            [b'r', b'#', ..] => {
                let token_end = eat_ident_cont(token_start.add(2));
                let acc = on_token(acc, TokenKind::RawIdent, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
            _ => {
                let token_end = eat_ident_cont(token_start.add(1));
                let acc = on_token(acc, TokenKind::Ident, token_start, token_end);
                become self.next_state(token_end.read())(self, acc, on_token, token_end, src_end)
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct ScalarTailCall {}
impl Lexer for ScalarTailCall {
    fn lex_bytes<B>(
        &self,
        bytes: &[u8],
        acc: B,
        mut on_token: impl FnMut(B, TokenKind, *const u8, *const u8) -> B,
    ) -> B {
        lex(bytes, acc, |acc, kind, start, end| {
            on_token(acc, kind, start, end)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integration() { crate::tests::integration_tests(ScalarTailCall {}); }
}
