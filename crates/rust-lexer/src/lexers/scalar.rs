//! Changes WRT to `rustc`:
//! * Iterates over bytes instead of Unicode chars
//! * Eliminate bounds checks by padding the input with `EOF_BYTE` (0xFF, cannot
//!   occur in valid UTF8).
//! * FIXME: handle Unicode whitespace and identifiers

#![allow(unsafe_op_in_unsafe_fn)]

use std::time::Duration;

use crate::common::{EOF_BYTE, Lexer, TokenKind};

pub fn lex(padded_src: &[u8], mut on_token: impl FnMut(TokenKind, *const u8, *const u8)) {
    let src = padded_src
        .strip_suffix(&[EOF_BYTE; 16])
        .expect("Input should be padded with EOF_BYTE");

    unsafe {
        let mut token_start = src.as_ptr();

        macro_rules! punct {
            ($kind:ident) => {{
                let token_end = token_start.add(1);
                on_token(TokenKind::$kind, token_start, token_end);
                token_start = token_end;
            }};
        }

        loop {
            match token_start.cast::<[u8; 4]>().read() {
                [b'(', ..] => punct!(LParen),
                [b')', ..] => punct!(RParen),
                [b'[', ..] => punct!(LSquare),
                [b']', ..] => punct!(RSquare),
                [b'{', ..] => punct!(LCurly),
                [b'}', ..] => punct!(RCurly),
                [b',', ..] => punct!(Comma),
                [b';', ..] => punct!(Semicolon),
                [b':', ..] => punct!(Colon),
                [b'+', ..] => punct!(Plus),
                [b'-', ..] => punct!(Minus),
                [b'*', ..] => punct!(Star),
                [b'%', ..] => punct!(Percent),
                [b'=', ..] => punct!(Eq),
                [b'&', ..] => punct!(And),
                [b'|', ..] => punct!(Or),
                [b'$', ..] => punct!(Dollar),
                [b'?', ..] => punct!(Question),
                [b'~', ..] => punct!(Tilde),
                [b'#', ..] => punct!(Hash),
                [b'@', ..] => punct!(At),
                [b'.', ..] => punct!(Dot),
                [b'!', ..] => punct!(Bang),
                [b'>', ..] => punct!(Gt),
                [b'<', ..] => punct!(Lt),
                [b'^', ..] => punct!(Caret),

                [b'/', b'/', ..] => {
                    let mut token_end = token_start.add(2);
                    while token_end.read() != b'\n' && token_end.read() != EOF_BYTE {
                        token_end = token_end.add(1);
                    }
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
                [b'/', ..] => punct!(Slash),

                [b'"', ..] => {
                    let end = double_quote_string(token_start);
                    on_token(TokenKind::Str, token_start, end);
                    token_start = end;
                }
                [b'\'', ..] => {
                    let mut end = token_start.add(1);
                    if let b'a'..=b'z' | b'A'..=b'Z' | b'_' = end.read() {
                        while let b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9' = end.read() {
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
                    let end = double_quote_string(token_start.add(1));
                    on_token(TokenKind::BStr, token_start, end);
                    token_start = end;
                }
                [b'b', b'r', b'"', ..] => {
                    let end = raw_string(token_start.add(1));
                    on_token(TokenKind::RawBStr, token_start, end);
                    token_start = end;
                }
                [b'b', b'r', b'#', ..] => {
                    let end = raw_hash_string(token_start.add(1));
                    on_token(TokenKind::RawBStr, token_start, end);
                    token_start = end;
                }

                [b'c', b'"', ..] => {
                    let end = double_quote_string(token_start.add(1));
                    on_token(TokenKind::CStr, token_start, end);
                    token_start = end;
                }
                [b'c', b'r', b'"', ..] => {
                    let end = raw_string(token_start.add(1));
                    on_token(TokenKind::RawCStr, token_start, end);
                    token_start = end;
                }
                [b'c', b'r', b'#', ..] => {
                    let end = raw_hash_string(token_start.add(1));
                    on_token(TokenKind::RawCStr, token_start, end);
                    token_start = end;
                }

                [b'r', b'"', ..] => {
                    let end = raw_string(token_start);
                    on_token(TokenKind::RawStr, token_start, end);
                    token_start = end;
                }
                [b'r', b'#', b'#' | b'"', ..] => {
                    let end = raw_hash_string(token_start);
                    on_token(TokenKind::RawStr, token_start, end);
                    token_start = end;
                }
                [b'r', b'#', ..] => {
                    let mut token_end = token_start.add(2);
                    while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = token_end.read() {
                        token_end = token_end.add(1);
                    }
                    on_token(TokenKind::RawIdent, token_start, token_end);
                    token_start = token_end;
                }

                [b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                    let mut token_end = token_start.add(1);
                    while matches!(token_end.read(), b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9')
                    {
                        token_end = token_end.add(1);
                    }
                    on_token(TokenKind::Ident, token_start, token_end);
                    token_start = token_end;
                }
                [b'0'..=b'9', ..] => {
                    let mut token_end = token_start.add(1);
                    while matches!(token_end.read(), b'0'..=b'9' | b'_') {
                        token_end = token_end.add(1);
                    }
                    let mut kind = match token_end.cast::<[u8; 2]>().read() {
                        [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                            on_token(TokenKind::Int, token_start, token_end);
                            token_start = token_end;
                            continue;
                        }
                        [b'.', ..] => {
                            token_end = token_end.add(1);
                            while let b'0'..=b'9' | b'_' = token_end.read() {
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

                    while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = token_end.read() {
                        token_end = token_end.add(1);
                    }
                    on_token(kind, token_start, token_end);
                    token_start = token_end;
                }

                [b' ' | 0x09..=0x0C, ..] => {
                    let mut token_end = token_start.add(1);
                    while let b' ' | 0x09..=0x0C = token_end.read() {
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

unsafe fn double_quote_string(start: *const u8) -> *const u8 {
    debug_assert_eq!(start.read(), b'\"');
    let mut end = start.add(1);
    loop {
        match end.read() {
            b'\\' => end = end.add(2),
            b'\"' => return end.add(1),
            EOF_BYTE => return end,
            _ => end = end.add(1),
        }
    }
}

unsafe fn raw_string(start: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"r\"");
    let mut end = start.add(2);
    loop {
        match end.read() {
            b'\"' => return end.add(1),
            EOF_BYTE => return end,
            _ => end = end.add(1),
        }
    }
}

unsafe fn raw_hash_string(start: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"r#");
    let mut end = start.add(2);
    let mut num_hashes = 1usize;
    while end.read() == b'#' {
        end = end.add(1);
        num_hashes += 1;
    }

    if end.read() != b'\"' {
        return end;
    }
    end = end.add(1);

    loop {
        match end.read() {
            b'\"' => {
                end = end.add(1);
                let mut num_hashes = num_hashes;
                while end.read() == b'#' {
                    end = end.add(1);
                    num_hashes -= 1;
                    if num_hashes == 0 {
                        return end;
                    }
                }
            }
            EOF_BYTE => return end,
            _ => end = end.add(1),
        }
    }
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
