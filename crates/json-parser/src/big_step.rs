use crate::Json;
use crate::Token;

pub fn parse(tokens: &[Token]) -> Result<(Json, &[Token]), (&'static str, &[Token])> {
    let [token, tokens1 @ ..] = tokens else {
        return Err(("unexpected EOF; expected JSON value", tokens));
    };

    match token {
        Token::True => Ok((Json::Bool(true), tokens1)),
        Token::False => Ok((Json::Bool(false), tokens1)),
        Token::Null => Ok((Json::Null, tokens1)),
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
    use super::*;

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse(&[Token::True]), Ok((Json::Bool(true), &[][..])));
        assert_eq!(parse(&[Token::False]), Ok((Json::Bool(false), &[][..])));
    }

    #[test]
    fn test_parse_null() {
        assert_eq!(parse(&[Token::Null]), Ok((Json::Null, &[][..])));
    }

    #[test]
    fn test_parse_empty_array() {
        assert_eq!(
            parse(&[Token::LSquare, Token::RSquare]),
            Ok((Json::Array(vec![]), &[][..]))
        );
    }

    #[test]
    fn test_parse_array_with_elements() {
        assert_eq!(
            parse(&[Token::LSquare, Token::True, Token::RSquare]),
            Ok((Json::Array(vec![Json::Bool(true)]), &[][..]))
        );
        assert_eq!(
            parse(&[
                Token::LSquare,
                Token::True,
                Token::Comma,
                Token::False,
                Token::RSquare
            ]),
            Ok((
                Json::Array(vec![Json::Bool(true), Json::Bool(false)]),
                &[][..]
            ))
        );
    }

    #[test]
    fn test_parse_empty_object() {
        assert_eq!(
            parse(&[Token::LCurly, Token::RCurly]),
            Ok((Json::Object(vec![]), &[][..]))
        );
    }

    #[test]
    fn test_parse_object_with_elements() {
        assert_eq!(
            parse(&[
                Token::LCurly,
                Token::String("key1".into()),
                Token::Colon,
                Token::True,
                Token::RCurly
            ]),
            Ok((
                Json::Object(vec![("key1".into(), Json::Bool(true))]),
                &[][..]
            ))
        );
        assert_eq!(
            parse(&[
                Token::LCurly,
                Token::String("key1".into()),
                Token::Colon,
                Token::True,
                Token::Comma,
                Token::String("key2".into()),
                Token::Colon,
                Token::False,
                Token::RCurly
            ]),
            Ok((
                Json::Object(vec![
                    ("key1".into(), Json::Bool(true)),
                    ("key2".into(), Json::Bool(false))
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
            parse(&[Token::RSquare]),
            Err((
                "unexpected token; expected JSON value",
                &[Token::RSquare][..]
            ))
        );
        assert_eq!(
            parse(&[Token::Comma]),
            Err(("unexpected token; expected JSON value", &[Token::Comma][..]))
        );
        assert_eq!(
            parse(&[Token::LSquare]),
            Err(("unexpected EOF; unterminated `[`", &[][..]))
        );
        assert_eq!(
            parse(&[Token::LSquare, Token::True]),
            Err(("unexpected EOF; expected `]` or `,`", &[][..]))
        );
        assert_eq!(
            parse(&[Token::LSquare, Token::True, Token::Comma]),
            Err(("unexpected EOF; expected JSON value", &[][..]))
        );
        assert_eq!(
            parse(&[Token::LCurly]),
            Err(("unexpected EOF; unterminated `{`", &[][..]))
        );
        assert_eq!(
            parse(&[Token::LCurly, Token::String("key1".into())]),
            Err(("unexpected EOF; expected `:`", &[][..]))
        );
        assert_eq!(
            parse(&[Token::LCurly, Token::String("key1".into()), Token::Colon]),
            Err(("unexpected EOF; expected JSON value", &[][..]))
        );
        assert_eq!(
            parse(&[
                Token::LCurly,
                Token::String("key1".into()),
                Token::Colon,
                Token::True
            ]),
            Err(("unexpected EOF; expected `}` or `,`", &[][..]))
        );
        assert_eq!(
            parse(&[
                Token::LCurly,
                Token::String("key1".into()),
                Token::Colon,
                Token::True,
                Token::Comma
            ]),
            Err(("unexpected EOF; expected string or `}`", &[][..]))
        );
    }
}
