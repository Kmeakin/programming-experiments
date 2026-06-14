#![allow(unsafe_op_in_unsafe_fn)]

use crate::common::{EOF_BYTE, Lexer, SIMD_PADDING, TokenKind};
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
        let src_end = padded_src.as_ptr_range().end.sub(SIMD_PADDING);
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
                            let token_end = line_comment(token_start, src_end);
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
