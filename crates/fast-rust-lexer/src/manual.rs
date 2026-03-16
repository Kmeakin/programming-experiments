use std::simd::prelude::*;

use crate::TokenKind;

pub fn lex_iter(input: &str) -> impl Iterator<Item = (TokenKind, u32)> {
    let mut input = input.as_bytes();

    std::iter::from_fn(move || {
        let (kind, len) = lex_one(input)?;
        input = unsafe { input.get_unchecked(len as usize..) };
        Some((kind, len))
    })
}

#[allow(clippy::cast_possible_truncation)]
fn lex_one(input: &[u8]) -> Option<(TokenKind, u32)> {
    debug_assert!(input.len() < u32::MAX as usize);

    let bytes0 = input;
    let [byte0, bytes1 @ ..] = bytes0 else {
        return None;
    };

    let (kind, len) = match byte0 {
        b' ' | b'\n' | b'\t' => (TokenKind::Whitespace, 1 + whitespace(bytes1)),
        b'(' => (TokenKind::OpenParen, 1),
        b')' => (TokenKind::CloseParen, 1),
        b'[' => (TokenKind::OpenBracket, 1),
        b']' => (TokenKind::CloseBracket, 1),
        b'{' => (TokenKind::OpenBrace, 1),
        b'}' => (TokenKind::CloseBrace, 1),
        b',' => (TokenKind::Comma, 1),
        b';' => (TokenKind::Semi, 1),
        b':' => (TokenKind::Colon, 1),

        b'+' => (TokenKind::Plus, 1),
        b'-' => (TokenKind::Minus, 1),
        b'*' => (TokenKind::Star, 1),
        b'%' => (TokenKind::Percent, 1),
        b'=' => (TokenKind::Eq, 1),
        b'&' => (TokenKind::And, 1),
        b'|' => (TokenKind::Or, 1),
        b'$' => (TokenKind::Dollar, 1),

        b'?' => (TokenKind::Question, 1),
        b'~' => (TokenKind::Tilde, 1),
        b'#' => (TokenKind::Hash, 1),
        b'@' => (TokenKind::At, 1),
        b'.' => (TokenKind::Dot, 1),
        b'!' => (TokenKind::Bang, 1),
        b'>' => (TokenKind::Gt, 1),
        b'<' => (TokenKind::Lt, 1),
        b'^' => (TokenKind::Caret, 1),

        b'/' => match bytes1 {
            [b'/', bytes2 @ ..] => (TokenKind::LineComment, 2 + line_comment(bytes2)),
            [b'*', bytes2 @ ..] => (TokenKind::BlockComment, 2 + block_comment(bytes2)),
            _ => (TokenKind::Slash, 1),
        },
        b'b' => match bytes1 {
            [b'\'', bytes2 @ ..] => (TokenKind::Byte, 2 + single_quote_string(bytes2)),
            [b'"', bytes2 @ ..] => (TokenKind::ByteStr, 2 + double_quote_string(bytes2)),
            [b'r', b'#', bytes3 @ ..] => (TokenKind::RawByteStr, 3 + hash_string(bytes3)),
            [b'r', b'"', bytes3 @ ..] => (TokenKind::RawByteStr, 3 + raw_string(bytes3)),
            _ => (TokenKind::Ident, ident(bytes0)),
        },
        b'c' => match bytes1 {
            [b'"', bytes2 @ ..] => (TokenKind::CStr, 2 + double_quote_string(bytes2)),
            [b'r', b'#', bytes3 @ ..] => (TokenKind::CStr, 3 + hash_string(bytes3)),
            [b'r', b'"', bytes3 @ ..] => (TokenKind::CStr, 3 + raw_string(bytes3)),
            _ => (TokenKind::Ident, ident(bytes0)),
        },
        b'r' => match bytes1 {
            [
                b'#',
                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_',
                bytes2 @ ..,
            ] => (TokenKind::RawIdent, 3 + ident(bytes2)),
            [b'#', bytes2 @ ..] => (TokenKind::RawStr, 2 + hash_string(bytes2)),
            [b'"', bytes2 @ ..] => (TokenKind::RawStr, 2 + raw_string(bytes2)),
            _ => (TokenKind::Ident, ident(bytes0)),
        },
        b'"' => (TokenKind::Str, 1 + double_quote_string(bytes1)),
        b'\'' => {
            let (kind, len) = char_or_lifetime(bytes1);
            (kind, 1 + len)
        }

        b'a'..=b'z' | b'A'..=b'Z' | b'_' => (TokenKind::Ident, ident(bytes0)),
        b'0'..=b'9' => number(bytes0),
        _ => (TokenKind::Unknown, 1),
    };
    Some((kind, len as u32))
}

fn whitespace(mut bytes: &[u8]) -> usize {
    let mut len = 0;

    while let [b' ' | b'\n' | b'\t', rest @ ..] = bytes {
        bytes = rest;
        len += 1;
    }

    len
}

fn line_comment(bytes: &[u8]) -> usize {
    match memchr::memchr(b'\n', bytes) {
        Some(pos) => pos,
        None => bytes.len(),
    }
}

fn block_comment(bytes: &[u8]) -> usize {
    let mut depth = 1u32;

    for pos in memchr::memchr_iter(b'/', bytes) {
        if pos > 0 && bytes.get(pos - 1) == Some(&b'*') {
            depth -= 1;
            if depth == 0 {
                return pos + 1;
            }
        } else if bytes.get(pos + 1) == Some(&b'*') {
            depth += 1;
        }
    }

    bytes.len()
}

fn single_quote_string(bytes: &[u8]) -> usize {
    let mut len = 0;
    let mut bytes = bytes;
    loop {
        match bytes {
            [b'\'', ..] => {
                len += 1;
                break;
            }
            | [] => break,
            [b'\\', _, rest @ ..] => {
                len += 2;
                bytes = rest;
            }
            [_, rest @ ..] => {
                len += 1;
                bytes = rest;
            }
        }
    }
    len
}

fn double_quote_string(bytes: &[u8]) -> usize {
    for pos in memchr::memchr_iter(b'"', bytes) {
        let backslashes = bytes[..pos]
            .iter()
            .rev()
            .take_while(|&&b| b == b'\\')
            .count();
        if backslashes % 2 == 0 {
            return pos + 1;
        }
    }
    bytes.len()
}

fn hash_string(mut bytes: &[u8]) -> usize {
    let orig_len = bytes.len();
    let mut num_hashes = 1;

    while let [b'#', rest @ ..] = bytes {
        num_hashes += 1;
        bytes = rest;
    }

    let [b'"', bytes @ ..] = bytes else {
        return bytes.len();
    };

    for pos in memchr::memchr_iter(b'"', bytes) {
        if bytes[pos + 1..]
            .iter()
            .take_while(|&&b| b == b'#')
            .take(num_hashes)
            .count()
            == num_hashes
        {
            return pos + num_hashes * 2 + 1;
        }
    }

    orig_len
}

fn raw_string(bytes: &[u8]) -> usize {
    match memchr::memchr(b'"', bytes) {
        Some(pos) => pos + 1,
        None => bytes.len(),
    }
}

fn ident(bytes: &[u8]) -> usize {
    let mut len = 0;

    let (chunks, mut bytes) = bytes.as_chunks::<16>();
    for chunk in chunks {
        let vec = Simd::from_array(*chunk);
        let mask = !((vec.simd_eq(Simd::splat(b'_')))
            | (Simd::splat(b'a').simd_le(vec) & vec.simd_le(Simd::splat(b'z')))
            | (Simd::splat(b'A').simd_le(vec) & vec.simd_le(Simd::splat(b'Z')))
            | (Simd::splat(b'0').simd_le(vec) & vec.simd_le(Simd::splat(b'9'))));

        match mask.first_set() {
            None => len += 16,
            Some(pos) => return len + pos,
        }
    }
    while let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', rest @ ..] = bytes {
        len += 1;
        bytes = rest;
    }
    len
}

fn number(mut bytes: &[u8]) -> (TokenKind, usize) {
    let mut len = 0;

    while let [b'0'..=b'9' | b'_', rest @ ..] = bytes {
        len += 1;
        bytes = rest;
    }

    let mut kind = match bytes {
        [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => TokenKind::Int,
        [b'.', rest @ ..] => {
            len += 1;
            bytes = rest;
            while let [b'0'..=b'9' | b'_', rest @ ..] = bytes {
                len += 1;
                bytes = rest;
            }

            TokenKind::Float
        }
        _ => TokenKind::Int,
    };

    if let [b'e' | b'E', rest @ ..] = bytes {
        kind = TokenKind::Float;

        len += 1;
        bytes = rest;

        if let [b'+' | b'-', rest @ ..] = bytes {
            len += 1;
            bytes = rest;
        }
    }

    len += ident(bytes);

    (kind, len)
}

fn char_or_lifetime(mut bytes1: &[u8]) -> (TokenKind, usize) {
    let mut len = 0;

    if let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', _, ..] = bytes1 {
        while let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', rest @ ..] = bytes1 {
            len += 1;
            bytes1 = rest;
        }

        match bytes1 {
            [b'\'', ..] => return (TokenKind::Char, len + 1),
            _ => return (TokenKind::Lifetime, len),
        }
    }

    loop {
        match bytes1 {
            [b'\'', ..] => {
                len += 1;
                break;
            }
            | [] => break,
            [b'\\', _, rest @ ..] => {
                len += 2;
                bytes1 = rest;
            }
            [_, rest @ ..] => {
                len += 1;
                bytes1 = rest;
            }
        }
    }
    (TokenKind::Char, len)
}

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use expect_test::expect;

    fn check(input: &str, expected: &Expect) {
        let mut start = 0;
        let actual = super::lex_iter(input)
            .map(|(kind, len)| {
                let end = start + len;
                let lexeme = &input[start as usize..end as usize];
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
