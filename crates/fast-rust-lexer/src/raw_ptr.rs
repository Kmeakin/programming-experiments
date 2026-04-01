use crate::{TokenKind, simdx::first_set};
use std::{ops::Range, simd::prelude::*};

type LexFn<F> = extern "rust-preserve-none" fn(&JumpTable<F>, F, *const u8, *const u8);

pub const EOF_BYTE: u8 = 0xff;
pub const VEC_LEN: usize = 16;
pub const EOF_PADDING: usize = VEC_LEN;

pub fn lex_loop<F: FnMut(TokenKind, *const u8, *const u8)>(src: &[u8], on_token: F) {
    debug_assert!(src.ends_with(&[EOF_BYTE; EOF_PADDING]));
    let table = const { &JumpTable::new() };
    let Range {
        start: src_start,
        end: src_end,
    } = src.as_ptr_range();
    let byte0 = unsafe { src_start.read() };
    table.fns[byte0 as usize](table, on_token, src_start, src_end);
}

pub fn hash_lengths(src: &str) -> usize {
    let mut hash = 0usize;
    lex_loop(src.as_bytes(), |kind, start, end| {
        let len = unsafe { end.offset_from_unsigned(start) };
        hash ^= len;
        hash ^= kind as usize;
    });
    hash
}

struct JumpTable<F> {
    fns: [LexFn<F>; 256],
}

#[rustfmt::skip]
macro_rules! def_wrapper {
    ($name:ident) => {
        extern "rust-preserve-none"
        fn $name(&self, mut on_token: F, token_start: *const u8, src_end: *const u8) {
            let (kind, token_end) = $name(token_start, src_end);
            on_token(kind, token_start, token_end);
            let next_token_start = token_end;
            let byte0 = unsafe { next_token_start.read() };
            become self.fns[byte0 as usize](self, on_token, next_token_start, src_end);
        }
    };
}

impl<F: FnMut(TokenKind, *const u8, *const u8)> JumpTable<F> {
    const fn new() -> Self {
        const {
            let mut fns: [LexFn<F>; 256] = [Self::unknown as LexFn<F>; 256];
            let mut i = 0;
            while i < 256 {
                fns[i] = match i as u8 {
                    b'(' => Self::open_paren,
                    b')' => Self::close_paren,
                    b'[' => Self::open_bracket,
                    b']' => Self::close_bracket,
                    b'{' => Self::open_brace,
                    b'}' => Self::close_brace,
                    b',' => Self::comma,
                    b';' => Self::semi,
                    b':' => Self::colon,
                    b'+' => Self::plus,
                    b'-' => Self::minus,
                    b'*' => Self::star,
                    b'%' => Self::percent,
                    b'=' => Self::eq,
                    b'&' => Self::and,
                    b'|' => Self::or,
                    b'$' => Self::dollar,
                    b'?' => Self::question,
                    b'~' => Self::tilde,
                    b'#' => Self::hash,
                    b'@' => Self::at,
                    b'.' => Self::dot,
                    b'!' => Self::bang,
                    b'>' => Self::gt,
                    b'<' => Self::lt,
                    b'^' => Self::caret,

                    b' ' | b'\t' | b'\n' | b'\r' => Self::whitespace,
                    b'/' => Self::slash_or_comment,

                    b'"' => Self::string,
                    b'\'' => Self::char_or_lifetime,
                    b'b' => Self::b_string_or_ident,
                    b'c' => Self::c_string_or_ident,
                    b'r' => Self::r_string_or_ident,

                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => Self::ident,
                    b'0'..=b'9' => Self::number,

                    EOF_BYTE => Self::eof,
                    _ => Self::unknown,
                };
                i += 1;
            }
            Self { fns }
        }
    }

    #[allow(clippy::unused_self)]
    extern "rust-preserve-none" fn eof(&self, _: F, _: *const u8, _: *const u8) {}
    extern "rust-preserve-none" fn unknown(
        &self,
        mut on_token: F,
        token_start: *const u8,
        src_end: *const u8,
    ) {
        let token_end = unsafe { token_start.add(1) };
        on_token(TokenKind::Unknown, token_start, token_end);

        let next_token_start = token_end;
        let byte0 = unsafe { next_token_start.read() };
        become self.fns[byte0 as usize](self, on_token, next_token_start, src_end);
    }

    def_wrapper!(open_paren);
    def_wrapper!(close_paren);
    def_wrapper!(open_bracket);
    def_wrapper!(close_bracket);
    def_wrapper!(open_brace);
    def_wrapper!(close_brace);
    def_wrapper!(comma);
    def_wrapper!(semi);
    def_wrapper!(colon);
    def_wrapper!(plus);
    def_wrapper!(minus);
    def_wrapper!(star);
    def_wrapper!(percent);
    def_wrapper!(eq);
    def_wrapper!(and);
    def_wrapper!(or);
    def_wrapper!(dollar);
    def_wrapper!(question);
    def_wrapper!(tilde);
    def_wrapper!(hash);
    def_wrapper!(at);
    def_wrapper!(dot);
    def_wrapper!(bang);
    def_wrapper!(gt);
    def_wrapper!(lt);
    def_wrapper!(caret);

    def_wrapper!(whitespace);
    def_wrapper!(slash_or_comment);
    def_wrapper!(string);
    def_wrapper!(char_or_lifetime);
    def_wrapper!(b_string_or_ident);
    def_wrapper!(c_string_or_ident);
    def_wrapper!(r_string_or_ident);
    def_wrapper!(ident);
    def_wrapper!(number);
}

#[rustfmt::skip]
macro_rules! def_punctuation {
    ($name:ident, $kind:ident) => {
        fn $name(token_start: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
            (TokenKind::$kind, unsafe { token_start.add(1) })
        }
    };
}

def_punctuation!(open_paren, OpenParen);
def_punctuation!(close_paren, CloseParen);
def_punctuation!(open_bracket, OpenBracket);
def_punctuation!(close_bracket, CloseBracket);
def_punctuation!(open_brace, OpenBrace);
def_punctuation!(close_brace, CloseBrace);
def_punctuation!(comma, Comma);
def_punctuation!(semi, Semi);
def_punctuation!(colon, Colon);
def_punctuation!(plus, Plus);
def_punctuation!(minus, Minus);
def_punctuation!(star, Star);
def_punctuation!(percent, Percent);
def_punctuation!(eq, Eq);
def_punctuation!(and, And);
def_punctuation!(or, Or);
def_punctuation!(dollar, Dollar);
def_punctuation!(question, Question);
def_punctuation!(tilde, Tilde);
def_punctuation!(hash, Hash);
def_punctuation!(at, At);
def_punctuation!(dot, Dot);
def_punctuation!(bang, Bang);
def_punctuation!(gt, Gt);
def_punctuation!(lt, Lt);
def_punctuation!(caret, Caret);

#[inline]
fn whitespace(mut cur: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
    unsafe {
        cur = cur.add(1);
        while let b' ' | b'\t' | b'\n' = cur.read() {
            cur = cur.add(1);
        }
        (TokenKind::Whitespace, cur)
    }
}

#[inline]
fn slash_or_comment(mut cur: *const u8, src_end: *const u8) -> (TokenKind, *const u8) {
    cur = unsafe { cur.add(1) };
    let byte1 = unsafe { cur.read() };
    unsafe {
        match byte1 {
            b'/' => (TokenKind::LineComment, eat_line_comment(cur.add(1))),
            b'*' => (
                TokenKind::BlockComment,
                eat_block_comment(cur.add(1), src_end),
            ),
            _ => (TokenKind::Slash, cur),
        }
    }
}
#[inline]
fn eat_line_comment(mut cur: *const u8) -> *const u8 {
    unsafe {
        loop {
            let vec = cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let mask = vec.simd_eq(Simd::splat(b'\n')) | vec.simd_eq(Simd::splat(EOF_BYTE));
            if let Some(off) = first_set(mask) {
                return cur.add(off);
            }
            cur = cur.add(VEC_LEN);
        }
    }
}
#[inline]
fn eat_block_comment(cur: *const u8, src_end: *const u8) -> *const u8 {
    let mut depth = 1usize;

    let haystack = unsafe { std::slice::from_ptr_range(cur..src_end) };

    for off in memchr::memchr_iter(b'/', haystack) {
        let cur = unsafe { cur.add(off) };
        let prev_byte = unsafe { cur.sub(1).read() };
        let next_byte = unsafe { cur.add(1).read() };

        if prev_byte == b'*' && off > 0 {
            depth -= 1;
            if depth == 0 {
                return unsafe { cur.add(1) };
            }
        } else if next_byte == b'*' {
            depth += 1;
        }
    }

    unsafe { src_end.sub(EOF_PADDING) }
}

#[inline]
fn char_or_lifetime(cur: *const u8, src_end: *const u8) -> (TokenKind, *const u8) {
    unsafe {
        let mut cur = cur.add(1);
        if let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = cur.read() {
            while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = cur.read() {
                cur = cur.add(1);
            }

            match cur.read() {
                b'\'' => return (TokenKind::Char, cur.add(1)),
                _ => return (TokenKind::Lifetime, cur),
            }
        }
    }

    (TokenKind::Char, eat_single_quote_string(cur, src_end))
}
#[inline]
fn string(cur: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
    (TokenKind::Str, eat_double_quote_string(cur))
}
#[inline]
fn b_string_or_ident(token_start: *const u8, src_end: *const u8) -> (TokenKind, *const u8) {
    unsafe {
        let cur = token_start.add(1);
        match cur.read() {
            b'\'' => return (TokenKind::Byte, eat_single_quote_string(cur, src_end)),
            b'\"' => return (TokenKind::ByteStr, eat_double_quote_string(cur)),
            b'r' => match cur.add(1).read() {
                b'\"' => return (TokenKind::RawByteStr, eat_raw_string(cur.add(1))),
                b'#' => return (TokenKind::RawByteStr, eat_hash_string(cur.add(2))),
                _ => {}
            },
            _ => {}
        }
        (TokenKind::Ident, eat_ident(token_start))
    }
}
#[inline]
fn c_string_or_ident(token_start: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
    unsafe {
        let cur = token_start.add(1);
        match cur.read() {
            b'\"' => return (TokenKind::CStr, eat_double_quote_string(cur)),
            b'r' => match cur.add(1).read() {
                b'\"' => return (TokenKind::RawCStr, eat_raw_string(cur.add(1))),
                b'#' => return (TokenKind::RawCStr, eat_hash_string(cur.add(2))),
                _ => {}
            },
            _ => {}
        }
        (TokenKind::Ident, eat_ident(token_start))
    }
}
#[inline]
fn r_string_or_ident(token_start: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
    unsafe {
        let cur = token_start.add(1);
        match cur.read() {
            b'\"' => return (TokenKind::RawStr, eat_raw_string(cur)),
            b'#' => match cur.add(1).read() {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    return (TokenKind::RawIdent, eat_ident(cur.add(2)));
                }
                _ => return (TokenKind::RawStr, eat_hash_string(cur.add(1))),
            },
            _ => {}
        }
        (TokenKind::Ident, eat_ident(token_start))
    }
}
#[inline]
fn eat_single_quote_string(token_start: *const u8, _src_end: *const u8) -> *const u8 {
    unsafe {
        let mut cur = token_start.add(1);
        loop {
            match cur.read() {
                b'\'' => return cur.add(1),
                b'\\' => match cur.add(1).read() {
                    EOF_BYTE => return cur.add(1),
                    _ => cur = cur.add(2),
                },
                EOF_BYTE => return cur,
                _ => cur = cur.add(1),
            }
        }
    }
}
#[inline]
fn eat_double_quote_string(mut cur: *const u8) -> *const u8 {
    unsafe {
        cur = cur.add(1);
        loop {
            let vec = cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let quote_mask = vec.simd_eq(Simd::splat(b'"'));
            let eof_mask = vec.simd_eq(Simd::splat(EOF_BYTE));
            if let Some(off) = first_set(quote_mask) {
                cur = cur.add(off).add(1);
                let mut backslashes = 0;
                let mut cur_back = cur.sub(2);
                while cur_back.read() == b'\\' {
                    backslashes += 1;
                    cur_back = cur_back.sub(1);
                }
                if backslashes % 2 == 0 {
                    return cur;
                }
                continue;
            }
            if let Some(off) = first_set(eof_mask) {
                return cur.add(off);
            }
            cur = cur.add(VEC_LEN);
        }
    }
}
#[inline]
fn eat_raw_string(mut cur: *const u8) -> *const u8 {
    unsafe {
        cur = cur.add(1);
        loop {
            let vec = cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let quote_mask = vec.simd_eq(Simd::splat(b'"'));
            let eof_mask = vec.simd_eq(Simd::splat(EOF_BYTE));
            if let Some(off) = first_set(quote_mask) {
                return cur.add(off).add(1);
            }
            if let Some(off) = first_set(eof_mask) {
                return cur.add(off);
            }
            cur = cur.add(VEC_LEN);
        }
    }
}
fn eat_hash_string(mut cur: *const u8) -> *const u8 {
    unsafe {
        let mut num_hashes = 1;
        while cur.read() == b'#' {
            num_hashes += 1;
            cur = cur.add(1);
        }

        let b'"' = cur.read() else {
            return cur;
        };
        cur = cur.add(1);

        loop {
            let vec = cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let quote_mask = vec.simd_eq(Simd::splat(b'"'));
            let eof_mask = vec.simd_eq(Simd::splat(EOF_BYTE));
            if let Some(off) = first_set(quote_mask) {
                cur = cur.add(off).add(1);
                let mut num_hashes = num_hashes;
                while cur.read() == b'#' {
                    cur = cur.add(1);
                    num_hashes -= 1;
                    if num_hashes == 0 {
                        return cur;
                    }
                }
                continue;
            }
            if let Some(off) = first_set(eof_mask) {
                return cur.add(off);
            }
            cur = cur.add(VEC_LEN);
        }
    }
}
#[inline]
fn ident(token_start: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
    (TokenKind::Ident, eat_ident(token_start))
}
#[inline]
fn eat_ident(mut cur: *const u8) -> *const u8 {
    unsafe {
        loop {
            let vec = cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let mask = (vec.simd_eq(Simd::splat(b'_')))
                | (Simd::splat(b'a').simd_le(vec) & vec.simd_le(Simd::splat(b'z')))
                | (Simd::splat(b'A').simd_le(vec) & vec.simd_le(Simd::splat(b'Z')))
                | (Simd::splat(b'0').simd_le(vec) & vec.simd_le(Simd::splat(b'9')));

            if let Some(off) = first_set(!mask) {
                return cur.add(off);
            }

            cur = cur.add(VEC_LEN);
        }
    }
}
#[inline]
fn eat_decimal(mut cur: *const u8) -> *const u8 {
    unsafe {
        loop {
            let vec = cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let mask = (vec.simd_eq(Simd::splat(b'_')))
                | (Simd::splat(b'0').simd_le(vec) & vec.simd_le(Simd::splat(b'9')));

            if let Some(off) = first_set(!mask) {
                return cur.add(off);
            }

            cur = cur.add(VEC_LEN);
        }
    }
}
#[inline]
fn number(cur: *const u8, _src_end: *const u8) -> (TokenKind, *const u8) {
    let mut cur = eat_decimal(cur);

    unsafe {
        let mut kind = match cur.read() {
            b'.' => match cur.add(1).read() {
                b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_' => TokenKind::Int,
                _ => {
                    cur = cur.add(1);
                    while let b'0'..=b'9' | b'_' = cur.read() {
                        cur = cur.add(1);
                    }
                    TokenKind::Float
                }
            },
            _ => TokenKind::Int,
        };

        if let b'e' | b'E' = cur.read() {
            kind = TokenKind::Float;
            cur = cur.add(1);

            if let b'+' | b'-' = cur.read() {
                cur = cur.add(1);
            }
        }

        (kind, eat_ident(cur))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::Expect;
    use expect_test::expect;
    use std::fmt::Write as _;

    #[track_caller]
    fn check(source: &str, expected: &Expect) {
        // Prepare the input
        let mut input = source.as_bytes().to_vec();
        input.extend_from_slice(&[EOF_BYTE; EOF_PADDING]);
        let input_start = input.as_ptr();

        let mut output = String::new();
        lex_loop(&input, |kind, start, end| {
            let start_pos = unsafe { start.offset_from_unsigned(input_start) };
            let end_pos = unsafe { end.offset_from_unsigned(input_start) };
            let span = start_pos..end_pos;
            let lexeme = &source[start_pos..end_pos];
            _ = writeln!(output, "({kind:?}, {span:?}, {lexeme:?})");
        });
        expected.assert_eq(output.trim());
    }

    #[test]
    fn empty() { check("", &expect![""]); }

    #[test]
    fn whitespace() {
        check(" \t\n", &expect![[r#"(Whitespace, 0..3, " \t\n")"#]]);
        check(" \t\n \t\n \t\n \t\n", &expect![[
            r#"(Whitespace, 0..12, " \t\n \t\n \t\n \t\n")"#
        ]]);
    }

    #[test]
    fn line_comments() {
        check("// line comment newline\n//line comment EOF", &expect![[
            r#"
            (LineComment, 0..23, "// line comment newline")
            (Whitespace, 23..24, "\n")
            (LineComment, 24..42, "//line comment EOF")"#
        ]]);
    }

    #[test]
    fn block_comments() {
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
                (BlockComment, 108..177, "/* /* unclosed block comment */\n            oh no, still in a comment")"#]],
        );

        check("/* EOF", &expect![[r#"(BlockComment, 0..6, "/* EOF")"#]]);
        check("/*/ EOF", &expect![[r#"(BlockComment, 0..7, "/*/ EOF")"#]]);
        check("/**/ EOF", &expect![[r#"
            (BlockComment, 0..4, "/**/")
            (Whitespace, 4..5, " ")
            (Ident, 5..8, "EOF")"#]]);
        check("/*// EOF", &expect![[
            r#"(BlockComment, 0..8, "/*// EOF")"#
        ]]);
        check("/*/* EOF", &expect![[
            r#"(BlockComment, 0..8, "/*/* EOF")"#
        ]]);
        check("/*/**/ EOF", &expect![[
            r#"(BlockComment, 0..10, "/*/**/ EOF")"#
        ]]);
        check("/*/**/ EOF", &expect![[
            r#"(BlockComment, 0..10, "/*/**/ EOF")"#
        ]]);
        check("/* /* */ */ EOF", &expect![[r#"
            (BlockComment, 0..11, "/* /* */ */")
            (Whitespace, 11..12, " ")
            (Ident, 12..15, "EOF")"#]]);
        check("/*/* */ */ EOF", &expect![[r#"
            (BlockComment, 0..10, "/*/* */ */")
            (Whitespace, 10..11, " ")
            (Ident, 11..14, "EOF")"#]]);
        check("/*/* */*/ EOF", &expect![[r#"
            (BlockComment, 0..9, "/*/* */*/")
            (Whitespace, 9..10, " ")
            (Ident, 10..13, "EOF")"#]]);
        check("/*/* */*/ EOF", &expect![[r#"
            (BlockComment, 0..9, "/*/* */*/")
            (Whitespace, 9..10, " ")
            (Ident, 10..13, "EOF")"#]]);
    }

    #[test]
    fn punctuation() {
        check("()[]{},;:", &expect![[r#"
            (OpenParen, 0..1, "(")
            (CloseParen, 1..2, ")")
            (OpenBracket, 2..3, "[")
            (CloseBracket, 3..4, "]")
            (OpenBrace, 4..5, "{")
            (CloseBrace, 5..6, "}")
            (Comma, 6..7, ",")
            (Semi, 7..8, ";")
            (Colon, 8..9, ":")"#]]);

        check("+-*%=&|$?~#@.!><^/", &expect![[r##"
            (Plus, 0..1, "+")
            (Minus, 1..2, "-")
            (Star, 2..3, "*")
            (Percent, 3..4, "%")
            (Eq, 4..5, "=")
            (And, 5..6, "&")
            (Or, 6..7, "|")
            (Dollar, 7..8, "$")
            (Question, 8..9, "?")
            (Tilde, 9..10, "~")
            (Hash, 10..11, "#")
            (At, 11..12, "@")
            (Dot, 12..13, ".")
            (Bang, 13..14, "!")
            (Gt, 14..15, ">")
            (Lt, 15..16, "<")
            (Caret, 16..17, "^")
            (Slash, 17..18, "/")"##]]);
    }

    #[test]
    fn identifiers() {
        check("a abcdefXYZ123 _ _foo _1 __1", &expect![[r#"
        (Ident, 0..1, "a")
        (Whitespace, 1..2, " ")
        (Ident, 2..14, "abcdefXYZ123")
        (Whitespace, 14..15, " ")
        (Ident, 15..16, "_")
        (Whitespace, 16..17, " ")
        (Ident, 17..21, "_foo")
        (Whitespace, 21..22, " ")
        (Ident, 22..24, "_1")
        (Whitespace, 24..25, " ")
        (Ident, 25..28, "__1")"#]]);
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
                (Float, 69..79, "1e+2suffix")"#]],
        );

        check(
            "0b10_1010asdfbz 0o755as_dfzxc 0xDEADBE_EFasdfzxc",
            &expect![[r#"
                (Int, 0..15, "0b10_1010asdfbz")
                (Whitespace, 15..16, " ")
                (Int, 16..29, "0o755as_dfzxc")
                (Whitespace, 29..30, " ")
                (Int, 30..48, "0xDEADBE_EFasdfzxc")"#]],
        );
    }

    #[test]
    fn lifetimes() {
        check("'a", &expect![[r#"(Lifetime, 0..2, "'a")"#]]);
        check("'abcdef_1234", &expect![[
            r#"(Lifetime, 0..12, "'abcdef_1234")"#
        ]]);

        check("'abcdef_1234 foo", &expect![[r#"
            (Lifetime, 0..12, "'abcdef_1234")
            (Whitespace, 12..13, " ")
            (Ident, 13..16, "foo")"#]]);

        check("'abcdef_1234'foo", &expect![[r#"
            (Char, 0..13, "'abcdef_1234'")
            (Ident, 13..16, "foo")"#]]);
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
            (Char, 16..21, "'foo'")"#]]);

        check("b'a' b'\n' b'\\'' b'' b'foo'", &expect![[r#"
            (Byte, 0..4, "b'a'")
            (Whitespace, 4..5, " ")
            (Byte, 5..9, "b'\n'")
            (Whitespace, 9..10, " ")
            (Byte, 10..15, "b'\\''")
            (Whitespace, 15..16, " ")
            (Byte, 16..19, "b''")
            (Whitespace, 19..20, " ")
            (Byte, 20..26, "b'foo'")"#]]);

        check(r"'!!!\", &expect![[r#"(Char, 0..5, "'!!!\\")"#]]);
        check(r"'!!!\\", &expect![[r#"(Char, 0..6, "'!!!\\\\")"#]]);
        check(r"'!!!\' EOF", &expect![[r#"(Char, 0..10, "'!!!\\' EOF")"#]]);
        check(r"'!!!\\' EOF", &expect![[r#"
            (Char, 0..7, "'!!!\\\\'")
            (Whitespace, 7..8, " ")
            (Ident, 8..11, "EOF")"#]]);
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
            (Str, 31..44, "\"unterminated")"#]
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
                (ByteStr, 34..48, "b\"unterminated")"#]],
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
                (CStr, 34..48, "c\"unterminated")"#]],
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
                (RawStr, 64..83, "r\"unterminated\n    ")"#]],
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
                (Whitespace, 0..5, "\n    ")
                (RawStr, 5..10, "r#\"\"#")
                (Whitespace, 10..15, "\n    ")
                (RawStr, 15..22, "r##\"\"##")
                (Whitespace, 22..27, "\n    ")
                (RawStr, 27..49, "r#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 49..54, "\n    ")
                (RawStr, 54..66, "r#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 66..71, "\n    ")
                (RawStr, 71..84, "r##\" ##\"\" \"##")
                (Whitespace, 84..89, "\n    ")
                (RawStr, 89..110, "r#\"unterminated\"\n    ")"###]],
        );

        check(r#"r#""#, &expect![[r#"(RawStr, 0..3, "r#\"")"#]]);
        check(r#"r#"""#, &expect![[r#"(RawStr, 0..4, "r#\"\"")"#]]);

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
                (RawByteStr, 94..116, "br#\"unterminated\"\n    ")"###]],
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
                (RawCStr, 94..116, "cr#\"unterminated\"\n    ")"###]],
        );
    }
}
