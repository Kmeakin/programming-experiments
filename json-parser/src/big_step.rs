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
        Token::String => Ok((Json::String, tokens1)),
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
        _ => Err((("unexpected token; expected JSON value"), tokens)),
    }
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
    }
}
