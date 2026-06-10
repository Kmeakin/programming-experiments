use crate::{Json, Token};

pub fn parse(tokens: &[Token]) -> Result<(Json, &[Token]), (&'static str, &[Token])> {
    let [token, tokens1 @ ..] = tokens else {
        return Err(("unexpected EOF; expected JSON value", tokens));
    };

    match token {
        Token::KwTrue => Ok((Json::Bool(true), tokens1)),
        Token::KwFalse => Ok((Json::Bool(false), tokens1)),
        Token::KwNull => Ok((Json::Null, tokens1)),
        Token::Number => Ok((Json::Number, tokens1)),
        Token::String(s) => Ok((Json::String(s.clone()), tokens1)),
        Token::LSquare => match tokens1 {
            [] => Err(("unexpected EOF; unterminated `[`", tokens1)),
            [Token::RSquare, rest @ ..] => Ok((Json::Array(vec![]), rest)),
            _ => {
                let (elem, mut tokens) = parse(tokens1)?;
                let mut elems = vec![elem];
                loop {
                    match tokens {
                        [Token::RSquare, tokens @ ..] => return Ok((Json::Array(elems), tokens)),
                        [Token::Comma, rest @ ..] => tokens = rest,
                        [_, tokens @ ..] => {
                            return Err(("unexpected token; expected `]` or `,`", tokens));
                        }
                        [] => return Err(("unexpected EOF; expected `]` or `,`", tokens)),
                    }

                    let (elem, rest) = parse(tokens)?;
                    elems.push(elem);
                    tokens = rest;
                }
            }
        },
        Token::LCurly => match tokens1 {
            [] => Err(("unexpected EOF; unterminated `{`", tokens1)),
            [Token::RCurly, rest @ ..] => Ok((Json::Object(vec![]), rest)),
            _ => {
                let (elem, mut tokens) = parse_kv_pair(tokens1)?;
                let mut elems = vec![elem];
                loop {
                    match tokens {
                        [Token::RCurly, tokens @ ..] => return Ok((Json::Object(elems), tokens)),
                        [Token::Comma, rest @ ..] => tokens = rest,
                        [_, tokens @ ..] => {
                            return Err(("unexpected token; expected `}` or `,`", tokens));
                        }
                        [] => return Err(("unexpected EOF; expected `}` or `,`", tokens)),
                    }

                    let (elem, rest) = parse_kv_pair(tokens)?;
                    elems.push(elem);
                    tokens = rest;
                }
            }
        },
        _ => Err((("unexpected token; expected JSON value"), tokens)),
    }
}

fn parse_kv_pair(tokens: &[Token]) -> Result<((String, Json), &[Token]), (&'static str, &[Token])> {
    let (key, mut tokens) = match tokens {
        [Token::String(key), tokens1 @ ..] => (key, tokens1),
        [_, ..] => return Err(("unexpected token; expected string or `}`", tokens)),
        [] => return Err(("unexpected EOF; expected string or `}`", tokens)),
    };

    match tokens {
        [Token::Colon, tokens2 @ ..] => tokens = tokens2,
        [_, ..] => return Err(("unexpected token; expected `:`", tokens)),
        [] => return Err(("unexpected EOF; expected `:`", tokens)),
    }

    let (value, tokens) = parse(tokens)?;
    let pair = (key.clone(), value);
    Ok((pair, tokens))
}

#[cfg(test)]
mod tests {
    use super::Json::*;
    use super::Token::*;
    use super::*;

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse(&[KwTrue]), Ok((Bool(true), &[][..])));
        assert_eq!(parse(&[KwFalse]), Ok((Bool(false), &[][..])));
    }

    #[test]
    fn test_parse_null() {
        assert_eq!(parse(&[KwNull]), Ok((Null, &[][..])));
    }

    #[test]
    fn test_parse_empty_array() {
        assert_eq!(parse(&[LSquare, RSquare]), Ok((Array(vec![]), &[][..])));
    }

    #[test]
    fn test_parse_array_with_elements() {
        assert_eq!(
            parse(&[LSquare, KwTrue, RSquare]),
            Ok((Array(vec![Bool(true)]), &[][..]))
        );
        assert_eq!(
            parse(&[LSquare, KwTrue, Comma, KwFalse, RSquare]),
            Ok((Array(vec![Bool(true), Bool(false)]), &[][..]))
        );
    }

    #[test]
    fn test_parse_empty_object() {
        assert_eq!(parse(&[LCurly, RCurly]), Ok((Object(vec![]), &[][..])));
    }

    #[test]
    fn test_parse_object_with_elements() {
        assert_eq!(
            parse(&[LCurly, Token::String("key1".into()), Colon, KwTrue, RCurly]),
            Ok((Object(vec![("key1".into(), Bool(true))]), &[][..]))
        );
        assert_eq!(
            parse(&[
                LCurly,
                Token::String("key1".into()),
                Colon,
                KwTrue,
                Comma,
                Token::String("key2".into()),
                Colon,
                KwFalse,
                RCurly
            ]),
            Ok((
                Object(vec![
                    ("key1".into(), Bool(true)),
                    ("key2".into(), Bool(false))
                ]),
                &[][..]
            ))
        );
    }

    #[test]
    fn test_parse_errors() {
        assert_eq!(
            parse(&[]),
            Err(("unexpected EOF; expected JSON value", &[][..]))
        );
        assert_eq!(
            parse(&[RSquare]),
            Err(("unexpected token; expected JSON value", &[RSquare][..]))
        );
        assert_eq!(
            parse(&[Comma]),
            Err(("unexpected token; expected JSON value", &[Comma][..]))
        );
        assert_eq!(
            parse(&[LSquare]),
            Err(("unexpected EOF; unterminated `[`", &[][..]))
        );
        assert_eq!(
            parse(&[LSquare, KwTrue]),
            Err(("unexpected EOF; expected `]` or `,`", &[][..]))
        );
        assert_eq!(
            parse(&[LSquare, KwTrue, Comma]),
            Err(("unexpected EOF; expected JSON value", &[][..]))
        );
        assert_eq!(
            parse(&[LCurly]),
            Err(("unexpected EOF; unterminated `{`", &[][..]))
        );
        assert_eq!(
            parse(&[LCurly, Token::String("key1".into())]),
            Err(("unexpected EOF; expected `:`", &[][..]))
        );
        assert_eq!(
            parse(&[LCurly, Token::String("key1".into()), Colon]),
            Err(("unexpected EOF; expected JSON value", &[][..]))
        );
        assert_eq!(
            parse(&[LCurly, Token::String("key1".into()), Colon, KwTrue]),
            Err(("unexpected EOF; expected `}` or `,`", &[][..]))
        );
        assert_eq!(
            parse(&[LCurly, Token::String("key1".into()), Colon, KwTrue, Comma]),
            Err(("unexpected EOF; expected string or `}`", &[][..]))
        );
    }
}
