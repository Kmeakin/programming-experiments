#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum TokenKind {
    // Trivia
    // $ @ ` \
    Error(Error),

    // space, horizontal tab, vertical tab (0x0b), form-feed (0x0c)
    HorizontalWhitespace,
    /// new-line
    VerticalWhitespace,

    // //
    LineComment,
    // /* */
    BlockComment,

    // Punctuators (delimiters)
    // [ ] ( ) { }
    // <: :> <% %>
    LSquare, RSquare,
    LParen, RParen,
    LCurly, RCurly,
    DigraphLSquare, DigraphRSquare,
    DigraphLCurly, DigraphRCurly,

    // Punctuators (preprocessor operations)
    // ## #
    // %:%: %:
    Hash,
    HashHash,
    DigraphHash,
    DigraphHashHash,

    // Punctuators (misc)
    // ... . -> ? , ; :: :
    DotDotDot,
    Dot,
    Arrow,
    Question,
    Comma,
    Semicolon,
    ColonColon,
    Colon,

    // Punctuators (arithmetic operators)
    // ++ += +
    // -- -= -
    // *= *
    // /= /
    // %= %
    PlusPlus, PlusEqual, Plus,
    MinusMinus, MinusEqual, Minus,
    StarEqual, Star,
    SlashEqual, Slash,
    PercentEqual, Percent,

    // Punctuators (logical operators)
    // && ||
    AndAnd, OrOr,

    // Punctuators (bitwise operators)
    // &= &
    // |= |
    // ^= ^
    // ~
    // <<= <<
    // >>= >>
    AndEqual, And,
    OrEqual, Or,
    CaretEqual, Caret,
    Tilde,
    LessLessEqual, LessLess,
    GreaterGreaterEqual, GreaterGreater,

    // Punctuators (comparison operators)
    // == =
    // != !
    // <= <
    // >= >
    EqualEqual, Equal,
    BangEqual, Bang,
    LessEqual, Less,
    GreaterEqual, Greater,

    // Literals
    // Taken from the C++23 standard, not C23, because the C++ presentation is easier to read.
    // pp-number := "."? digit pp-continue*
    // pp-continue-seq :=
    //  | exp-char sign-char
    //  | identifier-continue
    //  | "."
    //  | "'" digit
    //  | "'" nondigit
    // digit := [0-9]
    // identifier-continue := [a-zA-Z0-9_]
    // nondigit := [a-zA-Z_]
    // exp-char := [epEP]
    // sign-char := [\-\+]
    Number,
    String,
    Character,

    Identifier,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownCharacter,
    UnterminatedString,
    UnterminatedCharacter,
    UnterminatedBlockComment,
}

fn lex_one(text: &str) -> Option<(TokenKind, &str)> {
    let bytes = text.as_bytes();
    let (kind, rest) = match bytes {
        [] => return None,

        // Whitespace
        [b' ' | b'\t' | 0x0b | 0x0c, rest @ ..] => {
            let mut bytes = rest;
            while let [b' ' | b'\t' | b'\n' | 0x0b | 0x0c, rest @ ..] = bytes {
                bytes = rest;
            }
            (TokenKind::HorizontalWhitespace, bytes)
        }
        [b'\n', rest @ ..] => (TokenKind::VerticalWhitespace, rest),

        // Comments
        [b'/', b'/', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] | [b'\n', ..] => break (TokenKind::LineComment, bytes),
                    [_, rest @ ..] => bytes = rest,
                }
            }
        }
        [b'/', b'*', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] => break (TokenKind::Error(Error::UnterminatedBlockComment), bytes),
                    [b'*', b'/', rest @ ..] => break (TokenKind::BlockComment, rest),
                    [_, rest @ ..] => bytes = rest,
                }
            }
        }

        // Literals
        // characters
        [b'\'', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] => break (TokenKind::Error(Error::UnterminatedCharacter), bytes),
                    [b'"', rest @ ..] => break (TokenKind::Character, rest),
                    [b'\\', _, rest @ ..] | [_, rest @ ..] => bytes = rest,
                }
            }
        }

        // strings
        [b'"', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] => break (TokenKind::Error(Error::UnterminatedString), bytes),
                    [b'"', rest @ ..] => break (TokenKind::String, rest),
                    [b'\\', _, rest @ ..] | [_, rest @ ..] => bytes = rest,
                }
            }
        }

        // pp-numbers
        [b'.', b'0'..=b'9', rest @ ..] | [b'0'..=b'9', rest @ ..] => {
            let mut bytes = rest;
            loop {
                #[allow(unused_parens, clippy::match_same_arms)]
                // pp-continue-seq :=
                match bytes {
                    // exp-char sign-char
                    [(b'e' | b'p' | b'E' | b'P'), (b'-' | b'+'), rest @ ..] => bytes = rest,

                    // identifier-continue
                    [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', rest @ ..] => bytes = rest,

                    // "."
                    [b'.', rest @ ..] => bytes = rest,

                    // "'" digit
                    [b'\'', b'0'..=b'9', rest @ ..] => bytes = rest,

                    // "'" nondigit
                    [b'\'', (b'a'..=b'z' | b'A'..=b'Z' | b'_'), rest @ ..] => bytes = rest,

                    rest => break (TokenKind::Number, rest),
                }
            }
        }

        // identifiers
        [b'A'..=b'Z' | b'a'..=b'z' | b'_', rest @ ..] => {
            let mut bytes = rest;
            while let [b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_', rest @ ..] = bytes {
                bytes = rest;
            }
            (TokenKind::Identifier, bytes)
        }

        // Punctuators (brackets)
        [b'<', b':', rest @ ..] => (TokenKind::DigraphLSquare, rest),
        [b':', b'>', rest @ ..] => (TokenKind::DigraphRSquare, rest),
        [b'<', b'%', rest @ ..] => (TokenKind::DigraphLCurly, rest),
        [b'%', b'>', rest @ ..] => (TokenKind::DigraphRCurly, rest),
        [b'(', rest @ ..] => (TokenKind::LParen, rest),
        [b')', rest @ ..] => (TokenKind::RParen, rest),
        [b'[', rest @ ..] => (TokenKind::LSquare, rest),
        [b']', rest @ ..] => (TokenKind::RSquare, rest),
        [b'{', rest @ ..] => (TokenKind::LCurly, rest),
        [b'}', rest @ ..] => (TokenKind::RCurly, rest),

        // Punctuators (preprocessor operators)
        [b'%', b':', b'%', b':', rest @ ..] => (TokenKind::DigraphHashHash, rest),
        [b'%', b':', rest @ ..] => (TokenKind::DigraphHash, rest),
        [b'#', b'#', rest @ ..] => (TokenKind::HashHash, rest),
        [b'#', rest @ ..] => (TokenKind::Hash, rest),

        // Punctuators (misc)
        [b'.', b'.', b'.', rest @ ..] => (TokenKind::DotDotDot, rest),
        [b'.', rest @ ..] => (TokenKind::Dot, rest),
        [b'-', b'>', rest @ ..] => (TokenKind::Arrow, rest),
        [b'?', rest @ ..] => (TokenKind::Question, rest),
        [b',', rest @ ..] => (TokenKind::Comma, rest),
        [b';', rest @ ..] => (TokenKind::Semicolon, rest),
        [b':', b':', rest @ ..] => (TokenKind::ColonColon, rest),
        [b':', rest @ ..] => (TokenKind::Colon, rest),

        // Punctuators (arithmetic operators)
        [b'+', b'+', rest @ ..] => (TokenKind::PlusPlus, rest),
        [b'+', b'=', rest @ ..] => (TokenKind::PlusEqual, rest),
        [b'+', rest @ ..] => (TokenKind::Plus, rest),

        [b'-', b'-', rest @ ..] => (TokenKind::MinusMinus, rest),
        [b'-', b'=', rest @ ..] => (TokenKind::MinusEqual, rest),
        [b'-', rest @ ..] => (TokenKind::Minus, rest),

        [b'*', b'=', rest @ ..] => (TokenKind::StarEqual, rest),
        [b'*', rest @ ..] => (TokenKind::Star, rest),

        [b'/', b'=', rest @ ..] => (TokenKind::SlashEqual, rest),
        [b'/', rest @ ..] => (TokenKind::Slash, rest),

        [b'%', b'=', rest @ ..] => (TokenKind::PercentEqual, rest),
        [b'%', rest @ ..] => (TokenKind::Percent, rest),

        // Punctuators (logical operators)
        [b'&', b'&', rest @ ..] => (TokenKind::AndAnd, rest),
        [b'|', b'|', rest @ ..] => (TokenKind::OrOr, rest),

        // Punctuators (bitwise operators)
        [b'&', b'=', rest @ ..] => (TokenKind::AndEqual, rest),
        [b'&', rest @ ..] => (TokenKind::And, rest),
        [b'|', b'=', rest @ ..] => (TokenKind::OrEqual, rest),
        [b'|', rest @ ..] => (TokenKind::Or, rest),
        [b'^', b'=', rest @ ..] => (TokenKind::CaretEqual, rest),
        [b'^', rest @ ..] => (TokenKind::Caret, rest),
        [b'~', rest @ ..] => (TokenKind::Tilde, rest),
        [b'<', b'<', b'=', rest @ ..] => (TokenKind::LessLessEqual, rest),
        [b'<', b'<', rest @ ..] => (TokenKind::LessLess, rest),
        [b'>', b'>', b'=', rest @ ..] => (TokenKind::GreaterGreaterEqual, rest),
        [b'>', b'>', rest @ ..] => (TokenKind::GreaterGreater, rest),

        // Punctuators (comparison operators)
        [b'=', b'=', rest @ ..] => (TokenKind::EqualEqual, rest),
        [b'=', rest @ ..] => (TokenKind::Equal, rest),

        [b'!', b'=', rest @ ..] => (TokenKind::BangEqual, rest),
        [b'!', rest @ ..] => (TokenKind::Bang, rest),

        [b'<', b'=', rest @ ..] => (TokenKind::LessEqual, rest),
        [b'<', rest @ ..] => (TokenKind::Less, rest),

        [b'>', b'=', rest @ ..] => (TokenKind::GreaterEqual, rest),
        [b'>', rest @ ..] => (TokenKind::Greater, rest),

        [0x80..0xE0, rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), &rest[1..]),
        [0xE0..0xF0, rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), &rest[2..]),
        [0xF0..=0xFF, rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), &rest[3..]),

        [0x00..0x20 | b'$' | b'\\' | b'`' | b'@' | 0x7f, rest @ ..] => {
            (TokenKind::Error(Error::UnknownCharacter), rest)
        }
    };
    let rest = unsafe { str::from_utf8_unchecked(rest) };
    Some((kind, rest))
}

/// Performs "phase 3" of translation (lexing into preprocessing tokens).
///
/// Assumes phase 1 (multibyte characters to source character set)
/// and phase 2 (backslash+newline replaced with newline) have already been
/// performed
pub fn lex(mut text: &str) -> impl Iterator<Item = (TokenKind, &str)> {
    std::iter::from_fn(move || {
        let (kind, rest) = lex_one(text)?;
        let token_text = &text[..(text.len() - rest.len())];
        text = rest;
        Some((kind, token_text))
    })
}

#[cfg(test)]
mod tests {
    #![allow(clippy::needless_pass_by_value)]

    use expect_test::*;

    use super::*;

    #[track_caller]
    fn check(text: &str, expected: Expect) {
        let actual = lex(text)
            .map(|x| format!("{x:?}"))
            .collect::<Vec<_>>()
            .join("\n");
        expected.assert_eq(&actual);
    }

    #[test]
    fn empty() { check("", expect![""]); }

    #[test]
    fn trivia() {
        check(
            " \t\n\x0b\x0c// line comment\n/* block /* comment */ // line comment again",
            expect![[r#"
                (HorizontalWhitespace, " \t\n\u{b}\u{c}")
                (LineComment, "// line comment")
                (VerticalWhitespace, "\n")
                (BlockComment, "/* block /* comment */")
                (HorizontalWhitespace, " ")
                (LineComment, "// line comment again")"#]],
        );
    }

    #[test]
    fn number() {
        check("0123456789", expect![[r#"(Number, "0123456789")"#]]);
        check(".0.0..0...", expect![[r#"(Number, ".0.0..0...")"#]]);
        check("0abcdefXZY123_", expect![[r#"(Number, "0abcdefXZY123_")"#]]);
        check("0'a", expect![[r#"(Number, "0'a")"#]]);
        check("1..E+3.foo", expect![[r#"(Number, "1..E+3.foo")"#]]);
        check("0JBK", expect![[r#"(Number, "0JBK")"#]]);
        check("0JBK'k", expect![[r#"(Number, "0JBK'k")"#]]);
        check("0JBK'1", expect![[r#"(Number, "0JBK'1")"#]]);
        check("0JBK'_", expect![[r#"(Number, "0JBK'_")"#]]);
        check("0JBK'", expect![[r#"
            (Number, "0JBK")
            (Error(UnterminatedCharacter), "'")"#]]);
    }

    #[test]
    fn punctuators_delimiters() {
        check("[](){}<::><%%>", expect![[r#"
            (LSquare, "[")
            (RSquare, "]")
            (LParen, "(")
            (RParen, ")")
            (LCurly, "{")
            (RCurly, "}")
            (DigraphLSquare, "<:")
            (DigraphRSquare, ":>")
            (DigraphLCurly, "<%")
            (DigraphRCurly, "%>")"#]]);
    }

    #[test]
    fn punctuators_preprocessor_operators() {
        check("###%:%:%:", expect![[r###"
            (HashHash, "##")
            (Hash, "#")
            (DigraphHashHash, "%:%:")
            (DigraphHash, "%:")"###]]);
    }

    #[test]
    fn punctuators_misc() {
        check("....->?,;:::", expect![[r#"
            (DotDotDot, "...")
            (Dot, ".")
            (Arrow, "->")
            (Question, "?")
            (Comma, ",")
            (Semicolon, ";")
            (ColonColon, "::")
            (Colon, ":")"#]]);
    }

    #[test]
    fn punctuators_arithmetic_operators() {
        check("++--&*+-~!", expect![[r#"
            (PlusPlus, "++")
            (MinusMinus, "--")
            (And, "&")
            (Star, "*")
            (Plus, "+")
            (Minus, "-")
            (Tilde, "~")
            (Bang, "!")"#]]);
    }

    #[test]
    fn punctuators_logical_operators() {
        check("&&||", expect![[r#"
        (AndAnd, "&&")
        (OrOr, "||")"#]]);
    }

    #[test]
    fn punctuators_bitwise_operators() {
        check("&=&|==^=^~<<=<<>>=>>", expect![[r#"
        (AndEqual, "&=")
        (And, "&")
        (OrEqual, "|=")
        (Equal, "=")
        (CaretEqual, "^=")
        (Caret, "^")
        (Tilde, "~")
        (LessLessEqual, "<<=")
        (LessLess, "<<")
        (GreaterGreaterEqual, ">>=")
        (GreaterGreater, ">>")"#]]);
    }

    #[test]
    fn punctuators_comparison_operators() {
        check("===!=!<=<>=>", expect![[r#"
        (EqualEqual, "==")
        (Equal, "=")
        (BangEqual, "!=")
        (Bang, "!")
        (LessEqual, "<=")
        (Less, "<")
        (GreaterEqual, ">=")
        (Greater, ">")"#]]);
    }

    #[test]
    fn identifiers() {
        check("foo _ _1 foo_barBaz12345", expect![[r#"
            (Identifier, "foo")
            (HorizontalWhitespace, " ")
            (Identifier, "_")
            (HorizontalWhitespace, " ")
            (Identifier, "_1")
            (HorizontalWhitespace, " ")
            (Identifier, "foo_barBaz12345")"#]]);
    }

    #[test]
    fn directives() {
        check("#foo bar", expect![[r##"
            (Hash, "#")
            (Identifier, "foo")
            (HorizontalWhitespace, " ")
            (Identifier, "bar")"##]]);
    }

    #[test]
    fn include() {
        check(r#"#include"foo""#, expect![[r##"
            (Hash, "#")
            (Identifier, "include")
            (String, "\"foo\"")"##]]);

        check(r#"#include "foo""#, expect![[r##"
            (Hash, "#")
            (Identifier, "include")
            (HorizontalWhitespace, " ")
            (String, "\"foo\"")"##]]);

        check(r"#include<foo>", expect![[r##"
            (Hash, "#")
            (Identifier, "include")
            (Less, "<")
            (Identifier, "foo")
            (Greater, ">")"##]]);

        check(r"#include <foo>", expect![[r##"
            (Hash, "#")
            (Identifier, "include")
            (HorizontalWhitespace, " ")
            (Less, "<")
            (Identifier, "foo")
            (Greater, ">")"##]]);
    }

    #[test]
    fn embed() {
        check(r#"#embed"foo""#, expect![[r##"
            (Hash, "#")
            (Identifier, "embed")
            (String, "\"foo\"")"##]]);

        check(r#"#embed "foo""#, expect![[r##"
            (Hash, "#")
            (Identifier, "embed")
            (HorizontalWhitespace, " ")
            (String, "\"foo\"")"##]]);

        check(r"#embed<foo>", expect![[r##"
            (Hash, "#")
            (Identifier, "embed")
            (Less, "<")
            (Identifier, "foo")
            (Greater, ">")"##]]);

        check(r"#embed <foo>", expect![[r##"
            (Hash, "#")
            (Identifier, "embed")
            (HorizontalWhitespace, " ")
            (Less, "<")
            (Identifier, "foo")
            (Greater, ">")"##]]);
    }

    #[test]
    fn has_include() {
        check("#if __has_include (<foo>)", expect![[r##"
            (Hash, "#")
            (Identifier, "if")
            (HorizontalWhitespace, " ")
            (Identifier, "__has_include")
            (HorizontalWhitespace, " ")
            (LParen, "(")
            (Less, "<")
            (Identifier, "foo")
            (Greater, ">")
            (RParen, ")")"##]]);
    }
}
