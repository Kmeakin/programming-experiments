#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum TokenKind {
    Error(ErrorKind),

    // Trivia:
    HorizontalWhitespace,
    VerticalWhitespace,
    LineComment,
    BlockComment,

    // Delimiters:
    // [ ] ( ) { }
    LSquare, RSquare,
    LParen, RParen,
    LCurly, RCurly,

    // Punctuation:
    // . ? , ; :
    Dot,
    Question,
    Comma,
    Semicolon,
    Colon,

    // Arithmetic operators:
    // += +
    // -= -
    // *= *
    // /= /
    // %= %
    AddAssign, Add,
    SubAssign, Sub,
    MulAssign, Mul,
    DivAssign, Div,
    RemAssign, Rem,

    // Logical operators:
    // && ||
    LogicalAnd, LogicalOr,

    // Bitwise operators:
    // &= &
    // |= |
    // ^= ^
    // ~
    // <<= <<
    // >>= >>
    BitwiseAndAssign, BitwiseAnd,
    BitwiseOrAssign, BitwiseOr,
    BitwiseXorAssign, BitwiseXor,
    BitwiseNot,
    ShiftLeftAssign, ShiftLeft,
    ShiftRightAssign, GreaterGreater,

    // Comparison operators:
    // == =
    // != !
    // <= <
    // >= >
    Equal, Assign,
    NotEqual, LogicalNot,
    LessEqual, Less,
    GreaterEqual, Greater,

    // Literals:
    Integer,
    String,
    Character,

    Identifier,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    UnknownCharacter,
    UnterminatedString,
    UnterminatedCharacter,
    UnterminatedBlockComment,
    AsciiControl,
}

pub trait TokenSink {
    fn token(&mut self, kind: TokenKind, text: &str);
    fn error(&mut self, kind: ErrorKind, text: &str);
}

impl<Sink: TokenSink> TokenSink for &mut Sink {
    fn token(&mut self, kind: TokenKind, text: &str) { Sink::token(self, kind, text); }
    fn error(&mut self, kind: ErrorKind, text: &str) { Sink::error(self, kind, text); }
}

fn lex_one<Sink: TokenSink>(text: &str, mut sink: Sink) -> Option<&str> {
    #[allow(clippy::enum_glob_use)]
    use {ErrorKind::*, TokenKind::*};

    macro_rules! token {
        ($kind:expr, $rest:expr) => {{
            let rest = unsafe { str::from_utf8_unchecked($rest) };
            let token_text = &text[..(text.len() - rest.len())];
            sink.token($kind, token_text);
            Some(rest)
        }};
    }

    macro_rules! error {
        ($kind:expr, $rest:expr) => {{
            let rest = unsafe { str::from_utf8_unchecked($rest) };
            let token_text = &text[..(text.len() - rest.len())];
            sink.error($kind, token_text);
            Some(rest)
        }};
    }

    let bytes = text.as_bytes();
    match bytes {
        [] => None,

        // Whitespace
        [b' ' | b'\t', rest @ ..] => {
            let mut bytes = rest;
            while let [b' ' | b'\t', rest @ ..] = bytes {
                bytes = rest;
            }
            token!(HorizontalWhitespace, bytes)
        }
        [b'\n', rest @ ..] => token!(VerticalWhitespace, rest),

        // Comments
        [b'/', b'/', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] | [b'\n', ..] => break token!(LineComment, bytes),
                    [_, rest @ ..] => bytes = rest,
                }
            }
        }
        [b'/', b'*', rest @ ..] => {
            let mut bytes = rest;
            let mut depth = 1u32;
            loop {
                match bytes {
                    [] => break error!(UnterminatedBlockComment, bytes),
                    [b'/', b'*', rest @ ..] => {
                        bytes = rest;
                        depth += 1;
                    }
                    [b'*', b'/', rest @ ..] => {
                        bytes = rest;
                        depth -= 1;
                        if depth == 0 {
                            break token!(BlockComment, bytes);
                        }
                    }
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
                    [] => break error!(UnterminatedCharacter, bytes),
                    [b'"', rest @ ..] => break token!(Character, rest),
                    [b'\\', _, rest @ ..] | [_, rest @ ..] => bytes = rest,
                }
            }
        }

        // Strings
        [b'"', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] => break error!(UnterminatedString, bytes),
                    [b'"', rest @ ..] => break token!(String, rest),
                    [b'\\', _, rest @ ..] | [_, rest @ ..] => bytes = rest,
                }
            }
        }

        // Integers
        [b'0'..=b'9', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [b'0'..=b'9' | b'_', rest @ ..] => bytes = rest,
                    rest => break token!(Integer, rest),
                }
            }
        }

        // Identifiers
        [b'A'..=b'Z' | b'a'..=b'z' | b'_', rest @ ..] => {
            let mut bytes = rest;
            while let [b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_', rest @ ..] = bytes {
                bytes = rest;
            }
            token!(Identifier, bytes)
        }

        // Punctuation
        [b'(', rest @ ..] => token!(LParen, rest),
        [b')', rest @ ..] => token!(RParen, rest),
        [b'[', rest @ ..] => token!(LSquare, rest),
        [b']', rest @ ..] => token!(RSquare, rest),
        [b'{', rest @ ..] => token!(LCurly, rest),
        [b'}', rest @ ..] => token!(RCurly, rest),
        [b'.', rest @ ..] => token!(Dot, rest),
        [b'?', rest @ ..] => token!(Question, rest),
        [b',', rest @ ..] => token!(Comma, rest),
        [b';', rest @ ..] => token!(Semicolon, rest),
        [b':', rest @ ..] => token!(Colon, rest),

        // Arithmetic operators:
        [b'+', b'=', rest @ ..] => token!(AddAssign, rest),
        [b'+', rest @ ..] => token!(Add, rest),

        [b'-', b'=', rest @ ..] => token!(SubAssign, rest),
        [b'-', rest @ ..] => token!(Sub, rest),

        [b'*', b'=', rest @ ..] => token!(MulAssign, rest),
        [b'*', rest @ ..] => token!(Mul, rest),

        [b'/', b'=', rest @ ..] => token!(DivAssign, rest),
        [b'/', rest @ ..] => token!(Div, rest),

        [b'%', b'=', rest @ ..] => token!(RemAssign, rest),
        [b'%', rest @ ..] => token!(Rem, rest),

        // Logical operators:
        [b'&', b'&', rest @ ..] => token!(LogicalAnd, rest),
        [b'|', b'|', rest @ ..] => token!(LogicalOr, rest),

        // Bitwise operators:
        [b'&', b'=', rest @ ..] => token!(BitwiseAndAssign, rest),
        [b'&', rest @ ..] => token!(BitwiseAnd, rest),
        [b'|', b'=', rest @ ..] => token!(BitwiseOrAssign, rest),
        [b'|', rest @ ..] => token!(BitwiseOr, rest),
        [b'^', b'=', rest @ ..] => token!(BitwiseXorAssign, rest),
        [b'^', rest @ ..] => token!(BitwiseXor, rest),
        [b'~', rest @ ..] => token!(BitwiseNot, rest),
        [b'<', b'<', b'=', rest @ ..] => token!(ShiftLeftAssign, rest),
        [b'<', b'<', rest @ ..] => token!(ShiftLeft, rest),
        [b'>', b'>', b'=', rest @ ..] => token!(ShiftRightAssign, rest),
        [b'>', b'>', rest @ ..] => token!(GreaterGreater, rest),

        // Comparison operators:
        [b'=', b'=', rest @ ..] => token!(Equal, rest),
        [b'=', rest @ ..] => token!(Assign, rest),

        [b'!', b'=', rest @ ..] => token!(NotEqual, rest),
        [b'!', rest @ ..] => token!(LogicalNot, rest),

        [b'<', b'=', rest @ ..] => token!(LessEqual, rest),
        [b'<', rest @ ..] => token!(Less, rest),

        [b'>', b'=', rest @ ..] => token!(GreaterEqual, rest),
        [b'>', rest @ ..] => token!(Greater, rest),

        [..0x20 | 0x7f, rest @ ..] => error!(AsciiControl, rest),
        [0x80..0xE0, rest @ ..] => error!(UnknownCharacter, &rest[1..]),
        [0xE0..0xF0, rest @ ..] => error!(UnknownCharacter, &rest[2..]),
        [0xF0..=0xFF, rest @ ..] => error!(UnknownCharacter, &rest[3..]),

        [b'$' | b'\\' | b'`' | b'@' | b'#', rest @ ..] => {
            error!(UnknownCharacter, rest)
        }
    }
}

pub fn lex<Sink: TokenSink>(mut text: &str, mut sink: Sink) {
    while let Some(rest) = lex_one(text, &mut sink) {
        text = rest;
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::needless_pass_by_value)]

    use expect_test::*;

    use super::*;

    #[track_caller]
    fn check(text: &str, expected: Expect) {
        use std::fmt::Write;
        struct Sink(String);
        impl TokenSink for Sink {
            fn token(&mut self, kind: TokenKind, text: &str) {
                writeln!(self.0, "({kind:?}, {text:?})").unwrap();
            }

            fn error(&mut self, kind: ErrorKind, text: &str) {
                writeln!(self.0, "({kind:?}, {text:?})").unwrap();
            }
        }

        let mut sink = Sink(String::new());
        lex(text, &mut sink);
        expected.assert_eq(sink.0.trim());
    }

    #[test]
    fn empty() { check("", expect![""]); }

    #[test]
    fn trivia() {
        check("\x00foo\x7f", expect![[r#"
            (AsciiControl, "\0")
            (Identifier, "foo")
            (AsciiControl, "\u{7f}")"#]]);

        check(" \t\n// line comment\n// line comment again", expect![[
            r#"
            (HorizontalWhitespace, " \t")
            (VerticalWhitespace, "\n")
            (LineComment, "// line comment")
            (VerticalWhitespace, "\n")
            (LineComment, "// line comment again")"#
        ]]);

        check("/*", expect![[r#"(UnterminatedBlockComment, "/*")"#]]);
        check("/*/", expect![[r#"(UnterminatedBlockComment, "/*/")"#]]);
        check("/**/", expect![[r#"(BlockComment, "/**/")"#]]);
        check("/***/", expect![[r#"(BlockComment, "/***/")"#]]);
        check("//**", expect![[r#"(LineComment, "//**")"#]]);
        check("/**", expect![[r#"(UnterminatedBlockComment, "/**")"#]]);

        check("/**//", expect![[r#"
            (BlockComment, "/**/")
            (Div, "/")"#]]);

        check("/* nested /* block */ comment */", expect![[
            r#"(BlockComment, "/* nested /* block */ comment */")"#
        ]]);
        check("/* unterminated /* block */ comment ", expect![[
            r#"(UnterminatedBlockComment, "/* unterminated /* block */ comment ")"#
        ]]);
    }

    #[test]
    fn integer() {
        check("0123456789", expect![[r#"(Integer, "0123456789")"#]]);
        check("0123456789_123", expect![[
            r#"(Integer, "0123456789_123")"#
        ]]);
        check("0123456789__123", expect![[
            r#"(Integer, "0123456789__123")"#
        ]]);
        check("0123456789_", expect![[r#"(Integer, "0123456789_")"#]]);
        check("0123456789__", expect![[r#"(Integer, "0123456789__")"#]]);
    }

    #[test]
    fn punctuators_delimiters() {
        check("[](){}", expect![[r#"
            (LSquare, "[")
            (RSquare, "]")
            (LParen, "(")
            (RParen, ")")
            (LCurly, "{")
            (RCurly, "}")"#]]);
    }

    #[test]
    fn punctuators_misc() {
        check(".?,;:", expect![[r#"
            (Dot, ".")
            (Question, "?")
            (Comma, ",")
            (Semicolon, ";")
            (Colon, ":")"#]]);
    }

    #[test]
    fn punctuators_arithmetic_operators() {
        check("&*+-~!", expect![[r#"
            (BitwiseAnd, "&")
            (Mul, "*")
            (Add, "+")
            (Sub, "-")
            (BitwiseNot, "~")
            (LogicalNot, "!")"#]]);
    }

    #[test]
    fn punctuators_logical_operators() {
        check("&&||", expect![[r#"
            (LogicalAnd, "&&")
            (LogicalOr, "||")"#]]);
    }

    #[test]
    fn punctuators_bitwise_operators() {
        check("&=&|==^=^~<<=<<>>=>>", expect![[r#"
            (BitwiseAndAssign, "&=")
            (BitwiseAnd, "&")
            (BitwiseOrAssign, "|=")
            (Assign, "=")
            (BitwiseXorAssign, "^=")
            (BitwiseXor, "^")
            (BitwiseNot, "~")
            (ShiftLeftAssign, "<<=")
            (ShiftLeft, "<<")
            (ShiftRightAssign, ">>=")
            (GreaterGreater, ">>")"#]]);
    }

    #[test]
    fn punctuators_comparison_operators() {
        check("===!=!<=<>=>", expect![[r#"
            (Equal, "==")
            (Assign, "=")
            (NotEqual, "!=")
            (LogicalNot, "!")
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
}
