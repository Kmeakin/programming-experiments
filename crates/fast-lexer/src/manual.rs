#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Error,

    Whitespace,
    LineComment,

    // Balanced delimiters
    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,

    // Separators
    Comma,
    Semicolon,
    Colon,

    // Operators
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Percent,
    PercentEq,
    And,
    AndEq,
    AndAnd,
    Or,
    OrEq,
    OrOr,
    Caret,
    CaretEq,
    Eq,
    EqEq,
    Not,
    NotEq,
    Lt,
    LtEq,
    LtLt,
    LtLtEq,
    Gt,
    GtEq,
    GtGt,
    GtGtEq,
    Question,
    Tilde,

    Int,
    String,
    Char,
    Ident,

    // Misc
    Hash,
    At,
    Underscore,
}

fn lex_one(input: &str) -> Option<(TokenKind, usize)> {
    let bytes = input.as_bytes();
    let [b1, bytes @ ..] = bytes else {
        return None;
    };

    let mut bytes = bytes;
    let mut len = 1;
    let (kind, len) = match *b1 {
        b' ' | b'\n' | b'\t' => {
            while let [b' ' | b'\n' | b'\t', rest @ ..] = bytes {
                len += 1;
                bytes = rest;
            }
            (TokenKind::Whitespace, len)
        }
        b'(' => (TokenKind::LParen, 1),
        b')' => (TokenKind::RParen, 1),
        b'[' => (TokenKind::LSquare, 1),
        b']' => (TokenKind::RSquare, 1),
        b'{' => (TokenKind::LCurly, 1),
        b'}' => (TokenKind::RCurly, 1),
        b',' => (TokenKind::Comma, 1),
        b';' => (TokenKind::Semicolon, 1),
        b':' => (TokenKind::Colon, 1),
        b'+' => match bytes {
            [b'=', ..] => (TokenKind::PlusEq, 2),
            _ => (TokenKind::Plus, 1),
        },
        b'-' => match bytes {
            [b'=', ..] => (TokenKind::MinusEq, 2),
            _ => (TokenKind::Minus, 1),
        },
        b'*' => match bytes {
            [b'=', ..] => (TokenKind::StarEq, 2),
            _ => (TokenKind::Star, 1),
        },
        b'/' => match bytes {
            [b'=', ..] => (TokenKind::SlashEq, 2),
            [b'/', ..] => {
                while let [b, rest @ ..] = bytes {
                    len += 1;
                    bytes = rest;
                    if *b == b'\n' {
                        break;
                    }
                }
                (TokenKind::LineComment, len)
            }
            _ => (TokenKind::Slash, 1),
        },
        b'%' => match bytes {
            [b'=', ..] => (TokenKind::PercentEq, 2),
            _ => (TokenKind::Percent, 1),
        },
        b'&' => match bytes {
            [b'=', ..] => (TokenKind::AndEq, 2),
            [b'&', ..] => (TokenKind::AndAnd, 2),
            _ => (TokenKind::And, 1),
        },
        b'|' => match bytes {
            [b'|', ..] => (TokenKind::OrOr, 2),
            [b'=', ..] => (TokenKind::OrEq, 2),
            _ => (TokenKind::Or, 1),
        },
        b'^' => match bytes {
            [b'=', ..] => (TokenKind::CaretEq, 2),
            _ => (TokenKind::Caret, 1),
        },
        b'=' => match bytes {
            [b'=', ..] => (TokenKind::EqEq, 2),
            _ => (TokenKind::Eq, 1),
        },
        b'!' => match bytes {
            [b'=', ..] => (TokenKind::NotEq, 2),
            _ => (TokenKind::Not, 1),
        },
        b'<' => match bytes {
            [b'<', b'=', ..] => (TokenKind::LtLtEq, 3),
            [b'<', ..] => (TokenKind::LtLt, 2),
            [b'=', ..] => (TokenKind::LtEq, 2),
            _ => (TokenKind::Lt, 1),
        },
        b'>' => match bytes {
            [b'>', b'=', ..] => (TokenKind::GtGtEq, 3),
            [b'>', ..] => (TokenKind::GtGt, 2),
            [b'=', ..] => (TokenKind::GtEq, 2),
            _ => (TokenKind::Gt, 1),
        },
        b'?' => (TokenKind::Question, 1),
        b'~' => (TokenKind::Tilde, 1),
        b'#' => (TokenKind::Hash, 1),
        b'@' => (TokenKind::At, 1),
        b'_' => match bytes {
            [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', ..] => {
                while let [b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', rest @ ..] = bytes {
                    len += 1;
                    bytes = rest;
                }
                (TokenKind::Ident, len)
            }
            _ => (TokenKind::Underscore, 1),
        },
        b'0'..=b'9' => {
            while let [b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_', rest @ ..] = bytes {
                len += 1;
                bytes = rest;
            }
            (TokenKind::Int, len)
        }
        b'a'..=b'z' | b'A'..=b'Z' => {
            while let [b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_', rest @ ..] = bytes {
                len += 1;
                bytes = rest;
            }
            (TokenKind::Ident, len)
        }
        b'"' => {
            loop {
                match bytes {
                    [b'"', ..] => {
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
            (TokenKind::String, len)
        }
        b'\'' => {
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
            (TokenKind::Char, len)
        }
        _ => (TokenKind::Error, 1),
    };
    Some((kind, len))
}

pub fn lex_iter(
    mut input: &str,
) -> impl Iterator<Item = (TokenKind, std::ops::Range<usize>, &str)> {
    let mut pos = 0;
    std::iter::from_fn(move || {
        let (kind, len) = lex_one(input)?;
        let start = pos;
        let end = pos + len;
        let range = start..end;
        let substring = &input[..len];
        input = &input[len..];
        pos = end;
        Some((kind, range, substring))
    })
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;

    #[track_caller]
    fn check(input: &str, expected: &Expect) {
        let output = lex_iter(input)
            .map(|(kind, span, substring)| format!("{kind:?}({span:?}, {substring:?})"))
            .collect::<Vec<_>>()
            .join("\n");
        expected.assert_eq(&output);
    }

    #[test]
    fn empty() { check("", &expect![""]); }

    #[test]
    fn whitespace() { check(" \t\n", &expect![[r#"Whitespace(0..3, " \t\n")"#]]); }

    #[test]
    fn comments() {
        let input = "
            // Line comment newline
            // Line comment EOF";
        check(input, &expect![[r#"
            Whitespace(0..13, "\n            ")
            LineComment(13..37, "// Line comment newline\n")
            Whitespace(37..49, "            ")
            LineComment(49..68, "// Line comment EOF")"#]]);
    }

    #[test]
    fn delimiters() {
        check("()[]{}", &expect![[r#"
            LParen(0..1, "(")
            RParen(1..2, ")")
            LSquare(2..3, "[")
            RSquare(3..4, "]")
            LCurly(4..5, "{")
            RCurly(5..6, "}")"#]]);
    }

    #[test]
    fn separators() {
        check(",;:", &expect![[r#"
            Comma(0..1, ",")
            Semicolon(1..2, ";")
            Colon(2..3, ":")"#]]);
    }

    #[test]
    fn operators() {
        let input = "
            + += - -= * *= / /= % %=
            & &= && | |= || ^ ^=
            = == ! != < <= << <<= > >= >> >>=
            ? ~";
        check(input, &expect![[r#"
            Whitespace(0..13, "\n            ")
            Plus(13..14, "+")
            Whitespace(14..15, " ")
            PlusEq(15..17, "+=")
            Whitespace(17..18, " ")
            Minus(18..19, "-")
            Whitespace(19..20, " ")
            MinusEq(20..22, "-=")
            Whitespace(22..23, " ")
            Star(23..24, "*")
            Whitespace(24..25, " ")
            StarEq(25..27, "*=")
            Whitespace(27..28, " ")
            Slash(28..29, "/")
            Whitespace(29..30, " ")
            SlashEq(30..32, "/=")
            Whitespace(32..33, " ")
            Percent(33..34, "%")
            Whitespace(34..35, " ")
            PercentEq(35..37, "%=")
            Whitespace(37..50, "\n            ")
            And(50..51, "&")
            Whitespace(51..52, " ")
            AndEq(52..54, "&=")
            Whitespace(54..55, " ")
            AndAnd(55..57, "&&")
            Whitespace(57..58, " ")
            Or(58..59, "|")
            Whitespace(59..60, " ")
            OrEq(60..62, "|=")
            Whitespace(62..63, " ")
            OrOr(63..65, "||")
            Whitespace(65..66, " ")
            Caret(66..67, "^")
            Whitespace(67..68, " ")
            CaretEq(68..70, "^=")
            Whitespace(70..83, "\n            ")
            Eq(83..84, "=")
            Whitespace(84..85, " ")
            EqEq(85..87, "==")
            Whitespace(87..88, " ")
            Not(88..89, "!")
            Whitespace(89..90, " ")
            NotEq(90..92, "!=")
            Whitespace(92..93, " ")
            Lt(93..94, "<")
            Whitespace(94..95, " ")
            LtEq(95..97, "<=")
            Whitespace(97..98, " ")
            LtLt(98..100, "<<")
            Whitespace(100..101, " ")
            LtLtEq(101..104, "<<=")
            Whitespace(104..105, " ")
            Gt(105..106, ">")
            Whitespace(106..107, " ")
            GtEq(107..109, ">=")
            Whitespace(109..110, " ")
            GtGt(110..112, ">>")
            Whitespace(112..113, " ")
            GtGtEq(113..116, ">>=")
            Whitespace(116..129, "\n            ")
            Question(129..130, "?")
            Whitespace(130..131, " ")
            Tilde(131..132, "~")"#]]);
    }

    #[test]
    fn misc() {
        check("#@_", &expect![[r##"
            Hash(0..1, "#")
            At(1..2, "@")
            Underscore(2..3, "_")"##]]);
    }

    #[test]
    fn identifiers() {
        let input = "foo bar123 _baz";
        check(input, &expect![[r#"
            Ident(0..3, "foo")
            Whitespace(3..4, " ")
            Ident(4..10, "bar123")
            Whitespace(10..11, " ")
            Ident(11..15, "_baz")"#]]);
    }

    #[test]
    fn int_literals() {
        check("01234567890_12345__foo", &expect![[
            r#"Int(0..22, "01234567890_12345__foo")"#
        ]]);
    }

    #[test]
    fn string_literals() {
        check(r#""hello" "escapes \" \' \n \\" """#, &expect![[r#"
            String(0..7, "\"hello\"")
            Whitespace(7..8, " ")
            String(8..29, "\"escapes \\\" \\' \\n \\\\\"")
            Whitespace(29..30, " ")
            String(30..32, "\"\"")"#]]);
    }

    #[test]
    fn char_literals() {
        check(r#"'hello' 'escapes \" \' \n \\' ''"#, &expect![[r#"
            Char(0..7, "'hello'")
            Whitespace(7..8, " ")
            Char(8..29, "'escapes \\\" \\' \\n \\\\'")
            Whitespace(29..30, " ")
            Char(30..32, "''")"#]]);
    }
}
