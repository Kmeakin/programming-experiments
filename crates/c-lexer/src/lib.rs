// Punctuators:
// [ ] ( ) { } . ->
// ++ -- & * + - ~ !
// / % << >> < > <= >= == != ^ | && ||
// ? : :: ; ...
// = *= /= %= += -= <<= >>= &= ^= |=
// , # ##
// <: :> <% %> %: %:%:

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    LSquare,
    RSquare,
    LParen,
    RParen,
    LCurly,
    RCurly,
    Dot,
    Arrow,

    PlusPlus,
    MinusMinus,
    And,
    Star,
    Plus,
    Minus,
    Tilde,
    Bang,

    Slash,
    Percent,
    LessLess,
    GreaterGreater,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,
    Caret,
    Or,
    AndAnd,
    OrOr,

    Question,
    Colon,
    ColonColon,
    Semicolon,
    DotDotDot,

    Equal,
    StarEqual,
    SlashEqual,
    PercentEqual,
    PlusEqual,
    MinusEqual,
    LessLessEqual,
    GreaterGreaterEqual,
    AndEqual,
    CaretEqual,
    OrEqual,

    Comma,
    Hash,
    HashHash,

    // Atoms
    Identifier,
    Number,
    String,
    Character,

    // Trivia
    Whitespace,
    LineComment,
    BlockComment,

    Error(Error),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownCharacter,
    UnterminatedString,
    UnterminatedCharacter,
    UnterminatedBlockComment,
}

/// Assumes phase 1 (multibyte characters to source character set)
/// and phase 2 (backslash+newline replaced with newline) have already been
/// performed
fn lex_one(text: &str) -> Option<(TokenKind, &str)> {
    let bytes = text.as_bytes();
    let (kind, rest) = match bytes {
        [] => return None,
        [b' ' | b'\n' | b'\r' | b'\t' | 0x0b | 0x0c, rest @ ..] => {
            let mut bytes = rest;
            while let [b' ' | b'\n' | b'\r' | b'\t' | 0x0b | 0x0c, rest @ ..] = bytes {
                bytes = rest;
            }
            (TokenKind::Whitespace, bytes)
        }
        [b'!', b'=', rest @ ..] => (TokenKind::BangEqual, rest),
        [b'!', rest @ ..] => (TokenKind::Bang, rest),

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
        [b'#', b'#', rest @ ..] => (TokenKind::HashHash, rest),
        [b'#', rest @ ..] => (TokenKind::Hash, rest),

        [b'%', b'=', rest @ ..] => (TokenKind::PercentEqual, rest),
        [b'%', rest @ ..] => (TokenKind::Percent, rest),

        [b'&', b'=', rest @ ..] => (TokenKind::AndEqual, rest),
        [b'&', b'&', rest @ ..] => (TokenKind::AndAnd, rest),
        [b'&', rest @ ..] => (TokenKind::And, rest),

        [b'\'', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] => break (TokenKind::Error(Error::UnterminatedString), bytes),
                    [b'\'', rest @ ..] => break (TokenKind::Character, rest),
                    [_, rest @ ..] => bytes = rest,
                }
            }
        }

        [b'(', rest @ ..] => (TokenKind::LParen, rest),
        [b')', rest @ ..] => (TokenKind::RParen, rest),

        [b'*', b'=', rest @ ..] => (TokenKind::StarEqual, rest),
        [b'*', rest @ ..] => (TokenKind::Star, rest),

        [b'+', b'+', rest @ ..] => (TokenKind::PlusPlus, rest),
        [b'+', b'=', rest @ ..] => (TokenKind::PlusEqual, rest),
        [b'+', rest @ ..] => (TokenKind::Plus, rest),

        [b',', rest @ ..] => (TokenKind::Comma, rest),
        [b'-', b'-', rest @ ..] => (TokenKind::MinusMinus, rest),
        [b'-', b'=', rest @ ..] => (TokenKind::MinusEqual, rest),
        [b'-', rest @ ..] => (TokenKind::Minus, rest),

        [b'.', b'.', b'.', rest @ ..] => (TokenKind::DotDotDot, rest),
        [b'.', rest @ ..] => (TokenKind::Dot, rest),

        [b'/', b'=', rest @ ..] => (TokenKind::SlashEqual, rest),
        [b'/', b'/', rest @ ..] => {
            let mut bytes = rest;
            loop {
                match bytes {
                    [] => break (TokenKind::LineComment, bytes),
                    [b'\n', rest @ ..] => break (TokenKind::LineComment, rest),
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
        [b'/', rest @ ..] => (TokenKind::Slash, rest),

        [b'0'..=b'9', rest @ ..] => {
            let mut bytes = rest;
            while let [b'0'..=b'9', rest @ ..] = bytes {
                bytes = rest;
            }
            (TokenKind::Number, rest)
        }

        [b':', b':', rest @ ..] => (TokenKind::ColonColon, rest),
        [b':', rest @ ..] => (TokenKind::Colon, rest),

        [b';', rest @ ..] => (TokenKind::Semicolon, rest),

        [b'<', b'<', b'=', rest @ ..] => (TokenKind::LessLessEqual, rest),
        [b'<', b'<', rest @ ..] => (TokenKind::LessLess, rest),
        [b'<', b'=', rest @ ..] => (TokenKind::LessEqual, rest),
        [b'<', rest @ ..] => (TokenKind::Less, rest),

        [b'>', b'>', b'=', rest @ ..] => (TokenKind::GreaterGreaterEqual, rest),
        [b'>', b'>', rest @ ..] => (TokenKind::GreaterGreater, rest),
        [b'>', b'=', rest @ ..] => (TokenKind::GreaterEqual, rest),
        [b'>', rest @ ..] => (TokenKind::Greater, rest),

        [b'=', b'=', rest @ ..] => (TokenKind::EqualEqual, rest),
        [b'=', rest @ ..] => (TokenKind::Equal, rest),

        [b'?', rest @ ..] => (TokenKind::Question, rest),
        [b'@', rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), rest),

        [b'A'..=b'Z' | b'a'..=b'z' | b'_', rest @ ..] => {
            let mut bytes = rest;
            while let [b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_', rest @ ..] = bytes {
                bytes = rest;
            }
            (TokenKind::Identifier, bytes)
        }

        [b'[', rest @ ..] => (TokenKind::LSquare, rest),
        [b']', rest @ ..] => (TokenKind::RSquare, rest),

        [b'^', b'=', rest @ ..] => (TokenKind::CaretEqual, rest),
        [b'^', rest @ ..] => (TokenKind::Caret, rest),

        [b'|', b'=', rest @ ..] => (TokenKind::OrEqual, rest),
        [b'|', b'|', rest @ ..] => (TokenKind::OrOr, rest),
        [b'|', rest @ ..] => (TokenKind::Or, rest),

        [b'{', rest @ ..] => (TokenKind::LCurly, rest),
        [b'}', rest @ ..] => (TokenKind::RCurly, rest),
        [b'~', rest @ ..] => (TokenKind::Tilde, rest),

        [0x80..0xE0, rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), &rest[1..]),
        [0xE0..0xF0, rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), &rest[2..]),
        [0xF0..=0xFF, rest @ ..] => (TokenKind::Error(Error::UnknownCharacter), &rest[3..]),

        [0x00..0x20 | b'$' | b'\\' | b'`' | 0x7f, rest @ ..] => {
            (TokenKind::Error(Error::UnknownCharacter), rest)
        }
    };
    let rest = unsafe { str::from_utf8_unchecked(rest) };
    Some((kind, rest))
}

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
    fn identifiers() {
        check("foo _ _1 foo_barBaz12345", expect![[r#"
            (Identifier, "foo")
            (Whitespace, " ")
            (Identifier, "_")
            (Whitespace, " ")
            (Identifier, "_1")
            (Whitespace, " ")
            (Identifier, "foo_barBaz12345")"#]]);
    }

    #[test]
    fn punctuators() {
        check("[](){}.->", expect![[r#"
            (LSquare, "[")
            (RSquare, "]")
            (LParen, "(")
            (RParen, ")")
            (LCurly, "{")
            (RCurly, "}")
            (Dot, ".")
            (Minus, "-")
            (Greater, ">")"#]]);

        check("++--&*+-~!", expect![[r#"
            (PlusPlus, "++")
            (MinusMinus, "--")
            (And, "&")
            (Star, "*")
            (Plus, "+")
            (Minus, "-")
            (Tilde, "~")
            (Bang, "!")"#]]);

        check("/%<<>><><=>===!=^|&&||", expect![[r#"
            (Slash, "/")
            (Percent, "%")
            (LessLess, "<<")
            (GreaterGreater, ">>")
            (Less, "<")
            (Greater, ">")
            (LessEqual, "<=")
            (GreaterEqual, ">=")
            (EqualEqual, "==")
            (BangEqual, "!=")
            (Caret, "^")
            (Or, "|")
            (AndAnd, "&&")
            (OrOr, "||")"#]]);

        check(",###", expect![[r###"
            (Comma, ",")
            (HashHash, "##")
            (Hash, "#")"###]]);
    }
}
