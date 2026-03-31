use crate::{TokenKind, simdx::first_set};
use std::simd::prelude::*;

type LexFn<F> = extern "rust-preserve-none" fn(&JumpTable<F>, F, &[u8]);

struct JumpTable<F> {
    fns: [LexFn<F>; 256],
}

impl<F: FnMut(TokenKind, usize)> JumpTable<F> {
    #[inline]
    extern "rust-preserve-none" fn next(&self, on_token: F, input: &[u8]) {
        let [byte, rest @ ..] = input else { return };
        become self.fns[*byte as usize](self, on_token, rest);
    }
}

#[rustfmt::skip]
macro_rules! ret {
    ($jump_table:expr, $on_token:expr, $kind:expr, $input:expr, $output:expr) => {{
        let input = $input;
        let output = $output;
        let len = input.len() - output.len() + 1;
        $on_token($kind, len);
        become $jump_table.next($on_token, output);
    }};
}

#[rustfmt::skip]
macro_rules! single_fn {
    ($name:ident, $kind:ident) => {
        extern "rust-preserve-none"
        fn $name(&self, mut on_token: F, input: &[u8]) {
            ret!(self, on_token, TokenKind::$kind, input, input);
        }
    };
}

#[rustfmt::skip]
macro_rules! wrap {
    ($name:ident) => {
        extern "rust-preserve-none"
        fn $name(&self, mut on_token: F, input: &[u8]) {
            let (kind, output) = $name(input);
            ret!(self, on_token, kind, input, output);
        }
    }
}

impl<F: FnMut(TokenKind, usize)> JumpTable<F> {
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

                    b' ' | b'\n' | b'\t' => Self::whitespace,
                    b'/' => Self::slash_or_comment,

                    b'\'' => Self::char_or_lifetime,
                    b'"' => Self::string,

                    b'b' => Self::b_string_or_ident,
                    b'c' => Self::c_string_or_ident,
                    b'r' => Self::r_string_or_ident,

                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => Self::ident,
                    b'0'..=b'9' => Self::number,

                    _ => Self::unknown,
                };
                i += 1;
            }
            Self { fns }
        }
    }

    single_fn!(unknown, Unknown);
    single_fn!(open_paren, OpenParen);
    single_fn!(close_paren, CloseParen);
    single_fn!(open_bracket, OpenBracket);
    single_fn!(close_bracket, CloseBracket);
    single_fn!(open_brace, OpenBrace);
    single_fn!(close_brace, CloseBrace);
    single_fn!(comma, Comma);
    single_fn!(semi, Semi);
    single_fn!(colon, Colon);
    single_fn!(plus, Plus);
    single_fn!(minus, Minus);
    single_fn!(star, Star);
    single_fn!(percent, Percent);
    single_fn!(eq, Eq);
    single_fn!(and, And);
    single_fn!(or, Or);
    single_fn!(dollar, Dollar);
    single_fn!(question, Question);
    single_fn!(tilde, Tilde);
    single_fn!(hash, Hash);
    single_fn!(at, At);
    single_fn!(dot, Dot);
    single_fn!(bang, Bang);
    single_fn!(gt, Gt);
    single_fn!(lt, Lt);
    single_fn!(caret, Caret);

    wrap!(whitespace);
    wrap!(slash_or_comment);
    wrap!(char_or_lifetime);
    wrap!(string);
    wrap!(b_string_or_ident);
    wrap!(c_string_or_ident);
    wrap!(r_string_or_ident);
    wrap!(number);
    wrap!(ident);
}

pub fn collect(bytes: &[u8]) -> Vec<(TokenKind, u32)> {
    let mut tokens = Vec::with_capacity(bytes.len());
    let mut ptr: *mut (TokenKind, u32) = tokens.as_mut_ptr();

    lex_loop(bytes, move |kind, len| unsafe {
        ptr.write((kind, len as u32));
        ptr = ptr.add(1);
    });

    unsafe {
        let len = ptr.offset_from_unsigned(tokens.as_mut_ptr());
        tokens.set_len(len);
    }

    tokens
}

pub fn lex_loop<F: FnMut(TokenKind, usize)>(bytes: &[u8], on_token: F) {
    let Some((byte0, rest)) = bytes.split_first() else {
        return;
    };
    let jump_table = const { JumpTable::new() };
    jump_table.fns[*byte0 as usize](&jump_table, on_token, rest);
}

#[inline]
fn whitespace(input: &[u8]) -> (TokenKind, &[u8]) {
    let mut output = input;
    while let [b' ' | b'\t' | b'\n', rest @ ..] = output {
        output = rest;
    }
    (TokenKind::Whitespace, output)
}

#[inline]
fn slash_or_comment(input: &[u8]) -> (TokenKind, &[u8]) {
    #[inline]
    fn eat_line_comment(input: &[u8]) -> &[u8] {
        match memchr::memchr(b'\n', input) {
            Some(pos) => unsafe { input.get_unchecked(pos..) },
            None => &input[input.len()..],
        }
    }

    #[inline]
    fn eat_block_comment(input: &[u8]) -> &[u8] {
        let mut depth = 1usize;

        for pos in memchr::memchr_iter(b'/', input) {
            if pos > 0 && input.get(pos - 1) == Some(&b'*') {
                depth -= 1;
                if depth == 0 {
                    unsafe { return input.get_unchecked(pos + 1..) };
                }
            } else if input.get(pos + 1) == Some(&b'*') {
                depth += 1;
            }
        }

        &input[input.len()..]
    }

    match input {
        [b'/', rest @ ..] => (TokenKind::LineComment, eat_line_comment(rest)),
        [b'*', rest @ ..] => (TokenKind::BlockComment, eat_block_comment(rest)),
        _ => (TokenKind::Slash, input),
    }
}

#[inline]
fn char_or_lifetime(input: &[u8]) -> (TokenKind, &[u8]) {
    let mut output = input;
    if let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', _, ..] = output {
        while let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', rest @ ..] = output {
            output = rest;
        }

        match output {
            [b'\'', output @ ..] => return (TokenKind::Char, output),
            _ => return (TokenKind::Lifetime, output),
        };
    }

    (TokenKind::Char, eat_single_quote_string(input))
}

#[inline]
fn string(input: &[u8]) -> (TokenKind, &[u8]) { (TokenKind::Str, eat_double_quote_string(input)) }

#[inline]
fn eat_single_quote_string(input: &[u8]) -> &[u8] {
    let mut output = input;
    loop {
        match output {
            [b'\'', rest @ ..] => {
                output = rest;
                break;
            }
            [] => break,
            [b'\\', _, rest @ ..] | [_, rest @ ..] => output = rest,
        }
    }
    output
}

#[inline]
fn eat_double_quote_string(input: &[u8]) -> &[u8] {
    for pos in memchr::memchr_iter(b'"', input) {
        let backslashes = unsafe { input.get_unchecked(..pos) }
            .iter()
            .rev()
            .take_while(|&&b| b == b'\\')
            .count();
        if backslashes % 2 == 0 {
            unsafe { return input.get_unchecked(pos + 1..) };
        }
    }
    &input[input.len()..]
}

#[inline]
fn b_string_or_ident(input: &[u8]) -> (TokenKind, &[u8]) {
    match input {
        [b'\'', rest @ ..] => (TokenKind::Byte, eat_single_quote_string(rest)),
        [b'"', rest @ ..] => (TokenKind::ByteStr, eat_double_quote_string(rest)),
        [b'r', b'#', rest @ ..] => (TokenKind::RawByteStr, eat_hash_string(rest)),
        [b'r', b'"', rest @ ..] => (TokenKind::RawByteStr, eat_raw_string(rest)),
        _ => (TokenKind::Ident, eat_ident(input)),
    }
}

#[inline]
fn c_string_or_ident(input: &[u8]) -> (TokenKind, &[u8]) {
    match input {
        [b'"', rest @ ..] => (TokenKind::CStr, eat_double_quote_string(rest)),
        [b'r', b'#', rest @ ..] => (TokenKind::RawCStr, eat_hash_string(rest)),
        [b'r', b'"', rest @ ..] => (TokenKind::RawCStr, eat_raw_string(rest)),
        _ => (TokenKind::Ident, eat_ident(input)),
    }
}

#[inline]
fn r_string_or_ident(input: &[u8]) -> (TokenKind, &[u8]) {
    match input {
        [
            b'#',
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_',
            rest @ ..,
        ] => (TokenKind::RawIdent, eat_ident(rest)),
        [b'#', rest @ ..] => (TokenKind::RawStr, eat_hash_string(rest)),
        [b'"', rest @ ..] => (TokenKind::RawStr, eat_raw_string(rest)),
        _ => (TokenKind::Ident, eat_ident(input)),
    }
}

#[inline(always)]
fn eat_hash_string(input: &[u8]) -> &[u8] {
    let mut output = input;
    let mut num_hashes = 1;

    while let [b'#', rest @ ..] = output {
        num_hashes += 1;
        output = rest;
    }

    let [b'"', output @ ..] = output else {
        return output;
    };

    for pos in memchr::memchr_iter(b'"', output) {
        if unsafe { output.get_unchecked(pos + 1..) }
            .iter()
            .take_while(|&&b| b == b'#')
            .take(num_hashes)
            .count()
            == num_hashes
        {
            return unsafe { output.get_unchecked(pos + num_hashes + 1..) };
        }
    }

    &output[output.len()..]
}

#[inline]
fn eat_raw_string(input: &[u8]) -> &[u8] {
    match memchr::memchr(b'"', input) {
        Some(pos) => unsafe { input.get_unchecked(pos + 1..) },
        None => &input[input.len()..],
    }
}

#[inline]
fn eat_ident(input: &[u8]) -> &[u8] {
    let mut len = 0;

    let (chunks, mut output) = input.as_chunks::<16>();
    for chunk in chunks {
        let vec = Simd::from_array(*chunk);
        let mask = !((vec.simd_eq(Simd::splat(b'_')))
            | (Simd::splat(b'a').simd_le(vec) & vec.simd_le(Simd::splat(b'z')))
            | (Simd::splat(b'A').simd_le(vec) & vec.simd_le(Simd::splat(b'Z')))
            | (Simd::splat(b'0').simd_le(vec) & vec.simd_le(Simd::splat(b'9'))));

        match first_set(mask) {
            None => len += 16,
            Some(pos) => unsafe { return input.get_unchecked(len + pos..) },
        }
    }
    while let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', rest @ ..] = output {
        output = rest;
    }
    output
}

#[inline]
fn ident(input: &[u8]) -> (TokenKind, &[u8]) { (TokenKind::Ident, eat_ident(input)) }

#[inline]
fn number(input: &[u8]) -> (TokenKind, &[u8]) {
    let mut output = eat_decimal(input);

    let mut kind = match output {
        [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => TokenKind::Int,
        [b'.', rest @ ..] => {
            output = rest;
            while let [b'0'..=b'9' | b'_', rest @ ..] = output {
                output = rest;
            }

            TokenKind::Float
        }
        _ => TokenKind::Int,
    };

    if let [b'e' | b'E', rest @ ..] = output {
        kind = TokenKind::Float;
        output = rest;

        if let [b'+' | b'-', rest @ ..] = output {
            output = rest;
        }
    }

    (kind, eat_ident(output))
}

#[inline]
fn eat_decimal(input: &[u8]) -> &[u8] {
    let mut len = 0;
    let (chunks, rest) = input.as_chunks::<16>();
    for chunk in chunks {
        let vec = Simd::from_array(*chunk);
        let mask = !((Simd::splat(b'0').simd_le(vec) & vec.simd_le(Simd::splat(b'9')))
            | vec.simd_eq(Simd::splat(b'_')));

        match first_set(mask) {
            None => len += 16,
            Some(pos) => unsafe { return input.get_unchecked(len + pos..) },
        }
    }
    let mut output = rest;
    while let [b'0'..=b'9' | b'_', rest @ ..] = output {
        output = rest;
    }
    output
}

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use expect_test::expect;

    fn check(input: &str, expected: &Expect) {
        let mut start = 0;
        let mut tokens = Vec::new();
        super::lex_loop(input.as_bytes(), |kind, len| tokens.push((kind, len)));
        let actual = tokens
            .into_iter()
            .map(|(kind, len)| {
                let end = start + len;
                let lexeme = &input[start..end];
                let span = start..end;
                start = end;
                (kind, lexeme, span)
            })
            .map(|(kind, lexeme, span)| format!("({kind:?}, {span:?}, {lexeme:?})"))
            .collect::<Vec<_>>()
            .join("\n");
        expected.assert_eq(&actual);
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
    fn comments() {
        check("// line comment newline\n//line comment EOF", &expect![[
            r#"
            (LineComment, 0..23, "// line comment newline")
            (Whitespace, 23..24, "\n")
            (LineComment, 24..42, "//line comment EOF")"#
        ]]);

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
                (CStr, 44..59, "cr\"raw string\\\"")
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
                (CStr, 5..11, "cr#\"\"#")
                (Whitespace, 11..16, "\n    ")
                (CStr, 16..24, "cr##\"\"##")
                (Whitespace, 24..29, "\n    ")
                (CStr, 29..52, "cr#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 52..57, "\n    ")
                (CStr, 57..70, "cr#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 70..75, "\n    ")
                (CStr, 75..89, "cr##\" ##\"\" \"##")
                (Whitespace, 89..94, "\n    ")
                (CStr, 94..116, "cr#\"unterminated\"\n    ")"###]],
        );
    }
}
