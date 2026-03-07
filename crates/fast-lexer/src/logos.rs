use std::ops::Range;

use logos::Logos;

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[derive(Logos)]
#[logos(skip r"[ \t\n]+")]
pub enum TokenKind {
    Error,

    #[regex(r"//[^\n]*\n?", allow_greedy = true)] LineComment,

    // Balanced delimiters
    #[token("(")] LParen,
    #[token(")")] RParen,

    #[token("[")] LSquare,
    #[token("]")] RSquare,

    #[token("{")] LCurly,
    #[token("}")] RCurly,

    // Separators
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token(":")] Colon,

    // Operators
    #[token("+")] Plus,
    #[token("+=")] PlusEq,

    #[token("-")] Minus,
    #[token("-=")] MinusEq,

    #[token("*")] Star,
    #[token("*=")] StarEq,

    #[token("/")] Slash,
    #[token("/=")] SlashEq,

    #[token("%")] Percent,
    #[token("%=")] PercentEq,

    #[token("&")] And,
    #[token("&=")] AndEq,
    #[token("&&")] AndAnd,

    #[token("|")] Or,
    #[token("|=")] OrEq,
    #[token("||")] OrOr,

    #[token("^")] Caret,
    #[token("^=")] CaretEq,

    #[token("=")] Eq,
    #[token("==")] EqEq,

    #[token("!")] Not,
    #[token("!=")] NotEq,

    #[token("<")] Lt,
    #[token("<=")] LtEq,
    #[token("<<")] LtLt,
    #[token("<<=")] LtLtEq,

    #[token(">")] Gt,
    #[token(">=")] GtEq,
    #[token(">>")] GtGt,
    #[token(">>=")] GtGtEq,

    #[token("?")] Question,
    #[token("~")] Tilde,

    #[regex(r"[0-9][0-9a-zA-Z_]+")] Int,
    #[regex(r#""([^"\\]|\\.)*""#)] String,
    #[regex(r#"'([^'\\]|\\.)*'"#)] Char,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")] Ident,

    // Misc
    #[token("#")] Hash,
    #[token("@")] At,
    #[token("_", priority = 3)] Underscore,
}

#[allow(clippy::cast_possible_truncation)]
pub fn lex_iter(input: &str) -> impl Iterator<Item = (TokenKind, Range<u32>, &str)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");

    logos::Lexer::new(input).spanned().map(|(kind, span)| {
        let substring = &input[span.clone()];
        let span = span.start as u32..span.end as u32;
        let kind = match kind {
            Ok(kind) => kind,
            Err(()) => TokenKind::Error,
        };
        (kind, span, substring)
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
    fn whitespace() { check(" \t\n", &expect![""]); }

    #[test]
    fn comments() {
        let input = "
            // Line comment newline
            // Line comment EOF";
        check(input, &expect![[r#"
            LineComment(13..37, "// Line comment newline\n")
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
            Plus(13..14, "+")
            PlusEq(15..17, "+=")
            Minus(18..19, "-")
            MinusEq(20..22, "-=")
            Star(23..24, "*")
            StarEq(25..27, "*=")
            Slash(28..29, "/")
            SlashEq(30..32, "/=")
            Percent(33..34, "%")
            PercentEq(35..37, "%=")
            And(50..51, "&")
            AndEq(52..54, "&=")
            AndAnd(55..57, "&&")
            Or(58..59, "|")
            OrEq(60..62, "|=")
            OrOr(63..65, "||")
            Caret(66..67, "^")
            CaretEq(68..70, "^=")
            Eq(83..84, "=")
            EqEq(85..87, "==")
            Not(88..89, "!")
            NotEq(90..92, "!=")
            Lt(93..94, "<")
            LtEq(95..97, "<=")
            LtLt(98..100, "<<")
            LtLtEq(101..104, "<<=")
            Gt(105..106, ">")
            GtEq(107..109, ">=")
            GtGt(110..112, ">>")
            GtGtEq(113..116, ">>=")
            Question(129..130, "?")
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
            Ident(4..10, "bar123")
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
            String(8..29, "\"escapes \\\" \\' \\n \\\\\"")
            String(30..32, "\"\"")"#]]);
    }

    #[test]
    fn char_literals() {
        check(r#"'hello' 'escapes \" \' \n \\' ''"#, &expect![[r#"
            Char(0..7, "'hello'")
            Char(8..29, "'escapes \\\" \\' \\n \\\\'")
            Char(30..32, "''")"#]]);
    }
}
