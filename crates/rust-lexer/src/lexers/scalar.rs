//! Changes WRT to `rustc`:
//! * Iterates over bytes instead of Unicode chars
//! * Eliminate bounds checks by padding the input with `EOF_BYTE` (0xFF, cannot
//!   occur in valid UTF8).
//! * Use a LUT for character classification instead of branches.
//! * Use `memchr` for finding the end of line comments and strings.
//! * FIXME: handle Unicode whitespace and identifiers

use crate::common::{EOF_BYTE, Lexer, SIMD_PADDING, TokenKind};
use crate::utils::*;

/// Only exported for `cargo asm`. Don't actually call!
pub fn lex_soa(padded_src: &[u8], kinds: &mut Vec<TokenKind>, ends: &mut Vec<u32>) {
    let kinds_ptr = kinds.as_mut_ptr();
    let ends_ptr = ends.as_mut_ptr();
    lex(
        padded_src,
        (kinds_ptr, ends_ptr),
        |(kinds_ptr, ends_ptr), kind, _, end| unsafe {
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
                .strip_suffix(&[EOF_BYTE; SIMD_PADDING])
                .expect("Input should be padded with EOF_BYTE");
        }

        let src_start = padded_src.as_ptr();
        let src_end = padded_src.as_ptr_range().end.sub(16);
        let mut token_start = src_start;

        loop {
            match token_start.cast::<[u8; 4]>().read() {
                [b'/', b'/', ..] => {
                    let token_end = line_comment(token_start, src_end);
                    acc = on_token(acc, TokenKind::LineComment, token_start, token_end);
                    token_start = token_end;
                }
                [b'/', b'*', ..] => {
                    let end = block_comment(token_start, src_end);
                    acc = on_token(acc, TokenKind::BlockComment, token_start, end);
                    token_start = end;
                }
                #[rustfmt::skip]
                [b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-'
                      | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@'
                      | b'.' | b'!' | b'>' | b'<' | b'^', ..] => {
                    while let Some(kind) = is_punct(token_start.read()) {
                        acc = on_token(acc, kind, token_start, token_start.add(1));
                        token_start = token_start.add(1);
                    };
                }
                [b'/', ..] => {
                    let token_end = token_start.add(1);
                    acc = on_token(acc, TokenKind::Slash, token_start, token_end);
                    token_start = token_end;
                }

                [b'"', ..] => {
                    let end = double_quote_string(token_start, src_end);
                    acc = on_token(acc, TokenKind::Str, token_start, end);
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
                                acc = on_token(acc, TokenKind::Char, token_start, end);
                                token_start = end;
                            }
                            _ => {
                                acc = on_token(acc, TokenKind::Lifetime, token_start, end);
                                token_start = end;
                            }
                        }
                    } else {
                        end = single_quote_string(token_start);
                        acc = on_token(acc, TokenKind::Char, token_start, end);
                        token_start = end;
                    }
                }

                [b'b', b'\'', ..] => {
                    let end = single_quote_string(token_start.add(1));
                    acc = on_token(acc, TokenKind::BChar, token_start, end);
                    token_start = end;
                }
                [b'b', b'"', ..] => {
                    let end = double_quote_string(token_start.add(1), src_end);
                    acc = on_token(acc, TokenKind::BStr, token_start, end);
                    token_start = end;
                }
                [b'b', b'r', b'"', ..] => {
                    let end = raw_string(token_start.add(1), src_end);
                    acc = on_token(acc, TokenKind::RawBStr, token_start, end);
                    token_start = end;
                }
                [b'b', b'r', b'#', ..] => {
                    let end = raw_hash_string(token_start.add(1), src_end);
                    acc = on_token(acc, TokenKind::RawBStr, token_start, end);
                    token_start = end;
                }

                [b'c', b'"', ..] => {
                    let end = double_quote_string(token_start.add(1), src_end);
                    acc = on_token(acc, TokenKind::CStr, token_start, end);
                    token_start = end;
                }
                [b'c', b'r', b'"', ..] => {
                    let end = raw_string(token_start.add(1), src_end);
                    acc = on_token(acc, TokenKind::RawCStr, token_start, end);
                    token_start = end;
                }
                [b'c', b'r', b'#', ..] => {
                    let end = raw_hash_string(token_start.add(1), src_end);
                    acc = on_token(acc, TokenKind::RawCStr, token_start, end);
                    token_start = end;
                }

                [b'r', b'"', ..] => {
                    let end = raw_string(token_start, src_end);
                    acc = on_token(acc, TokenKind::RawStr, token_start, end);
                    token_start = end;
                }
                [b'r', b'#', b'#' | b'"', ..] => {
                    let end = raw_hash_string(token_start, src_end);
                    acc = on_token(acc, TokenKind::RawStr, token_start, end);
                    token_start = end;
                }
                [b'r', b'#', ..] => {
                    let token_end = eat_ident_cont(token_start.add(2));
                    acc = on_token(acc, TokenKind::RawIdent, token_start, token_end);
                    token_start = token_end;
                }

                [b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                    let token_end = eat_ident_cont(token_start.add(1));
                    acc = on_token(acc, TokenKind::Ident, token_start, token_end);
                    token_start = token_end;
                }
                [b'0'..=b'9', ..] => {
                    let mut token_end = token_start.add(1);
                    token_end = eat_digits(token_end);
                    let mut kind = match token_end.cast::<[u8; 2]>().read() {
                        [b'.', b'.', ..] => {
                            acc = on_token(acc, TokenKind::Int, token_start, token_end);
                            token_start = token_end;
                            continue;
                        }
                        [b'.', b, ..] if is_ident_start(b) => {
                            acc = on_token(acc, TokenKind::Int, token_start, token_end);
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
                    acc = on_token(acc, kind, token_start, token_end);
                    token_start = token_end;
                }

                [b' ' | 0x09..=0x0C, ..] => {
                    let token_end = eat_whitespace(token_start.add(1));
                    acc = on_token(acc, TokenKind::Whitespace, token_start, token_end);
                    token_start = token_end;
                }

                [EOF_BYTE, ..] => return acc,

                _ => {
                    let end = token_start.add(1);
                    acc = on_token(acc, TokenKind::Unknown, token_start, end);
                    token_start = end;
                }
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct Scalar {}
impl Lexer for Scalar {
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
    fn integration() { crate::tests::integration_tests(Scalar {}); }
}
