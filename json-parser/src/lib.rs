pub mod big_step;
pub mod cps;
pub mod defunctionalised_cps;
pub mod double_barrelled_cps;
pub mod explicit_stack;
pub mod stack_allocated_cps;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    True,
    False,
    Null,
    Number,
    String(String),

    LSquare,
    RSquare,
    LCurly,
    RCurly,

    Comma,
    Colon,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme {
    Token(Token),
    Error(&'static str),
    Whitespace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Json {
    Bool(bool),
    Null,
    Number,
    String(String),
    Array(Vec<Self>),
    Object(Vec<(String, Self)>),
}

pub fn lex(input: &str) -> Option<(Lexeme, &str)> {
    let bytes0 = input.as_bytes();
    let [byte, bytes1 @ ..] = bytes0 else {
        return None;
    };

    let (lexeme, rest) = match byte {
        b' ' | b'\t' | b'\n' => {
            let mut bytes = bytes1;
            loop {
                match bytes {
                    [b' ' | b'\t' | b'\n', rest @ ..] => bytes = rest,
                    _ => break (Lexeme::Whitespace, bytes),
                }
            }
        }
        b't' => match bytes0.strip_prefix(b"true") {
            None => (Lexeme::Error("unterminated `true`"), bytes1),
            Some(rest) => (Lexeme::Token(Token::True), rest),
        },
        b'f' => match bytes0.strip_prefix(b"false") {
            None => (Lexeme::Error("unterminated `false`"), bytes1),
            Some(rest) => (Lexeme::Token(Token::False), rest),
        },
        b'n' => match bytes0.strip_prefix(b"null") {
            None => (Lexeme::Error("unterminated `null`"), bytes1),
            Some(rest) => (Lexeme::Token(Token::Null), rest),
        },
        b'"' => {
            let mut bytes = bytes1;
            loop {
                match bytes {
                    [] => break (Lexeme::Error("unterminated string"), bytes),
                    [b'"', rest @ ..] => break (Lexeme::Token(Token::String("".into())), rest),
                    [_, rest @ ..] => bytes = rest,
                }
            }
        }
        b'[' => (Lexeme::Token(Token::LSquare), bytes1),
        b']' => (Lexeme::Token(Token::RSquare), bytes1),
        b'{' => (Lexeme::Token(Token::LCurly), bytes1),
        b'}' => (Lexeme::Token(Token::RCurly), bytes1),
        b',' => (Lexeme::Token(Token::Comma), bytes1),
        b':' => (Lexeme::Token(Token::Colon), bytes1),
        ..0x80 => (Lexeme::Error("unknown token"), bytes1),
        ..0xE0 => (Lexeme::Error("unknown token"), &bytes1[1..]),
        ..0xF0 => (Lexeme::Error("unknown token"), &bytes1[2..]),
        _ => (Lexeme::Error("unknown token"), &bytes1[3..]),
    };
    let rest = unsafe { str::from_utf8_unchecked(rest) };
    Some((lexeme, rest))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lex_whitespace() {
        assert_eq!(lex("   \t\n  "), Some((Lexeme::Whitespace, "")));
        assert_eq!(lex("  true"), Some((Lexeme::Whitespace, "true")));
    }

    #[test]
    fn test_lex_keywords() {
        assert_eq!(lex("true"), Some((Lexeme::Token(Token::True), "")));
        assert_eq!(lex("false"), Some((Lexeme::Token(Token::False), "")));
        assert_eq!(lex("null"), Some((Lexeme::Token(Token::Null), "")));
    }

    #[test]
    fn test_lex_string() {
        assert_eq!(
            lex("\"hello\""),
            Some((Lexeme::Token(Token::String("".into())), ""))
        );
        assert_eq!(
            lex("\"\""),
            Some((Lexeme::Token(Token::String("".into())), ""))
        );
        assert_eq!(
            lex("\"unterminated"),
            Some((Lexeme::Error("unterminated string"), ""))
        );
    }

    #[test]
    fn test_lex_brackets() {
        assert_eq!(lex("["), Some((Lexeme::Token(Token::LSquare), "")));
        assert_eq!(lex("]"), Some((Lexeme::Token(Token::RSquare), "")));
        assert_eq!(lex("{"), Some((Lexeme::Token(Token::LCurly), "")));
        assert_eq!(lex("}"), Some((Lexeme::Token(Token::RCurly), "")));
    }

    #[test]
    fn test_lex_punctuation() {
        assert_eq!(lex(","), Some((Lexeme::Token(Token::Comma), "")));
        assert_eq!(lex(":"), Some((Lexeme::Token(Token::Colon), "")));
    }

    #[test]
    fn test_lex_errors() {
        assert_eq!(
            lex("tru"),
            Some((Lexeme::Error("unterminated `true`"), "ru"))
        );
        assert_eq!(
            lex("fals"),
            Some((Lexeme::Error("unterminated `false`"), "als"))
        );
        assert_eq!(
            lex("nul"),
            Some((Lexeme::Error("unterminated `null`"), "ul"))
        );
    }
}
