use crate::{Json, Token};

type PResult<'a, T> = Result<(T, &'a [Token]), (&'static str, &'a [Token])>;
type Cont<Arg, Ret> = Box<
    dyn for<'a> FnOnce(
        Result<(Arg, &'a [Token]), (&'static str, &'a [Token])>,
    ) -> Result<(Ret, &'a [Token]), (&'static str, &'a [Token])>,
>;

fn value<T: 'static>(tokens: &[Token], k: Cont<Json, T>) -> PResult<'_, T> {
    let [token, rest @ ..] = tokens else {
        return k(Err(("unexpected EOF; expected JSON value", tokens)));
    };

    match token {
        Token::KwTrue => k(Ok((Json::Bool(true), rest))),
        Token::KwFalse => k(Ok((Json::Bool(false), rest))),
        Token::KwNull => k(Ok((Json::Null, rest))),
        Token::Number => k(Ok((Json::Number, rest))),
        Token::String(s) => k(Ok((Json::String(s.clone()), rest))),
        Token::LSquare => match rest {
            [] => k(Err(("unexpected EOF; unterminated `[`", rest))),
            [Token::RSquare, rest @ ..] => k(Ok((Json::Array(vec![]), rest))),
            _ => value(
                rest,
                Box::new(|res| match res {
                    Err(e) => k(Err(e)),
                    Ok((elem, tokens)) => array_loop(vec![elem], tokens, k),
                }),
            ),
        },
        Token::LCurly => match rest {
            [] => k(Err(("unexpected EOF; unterminated `{`", rest))),
            [Token::RCurly, rest @ ..] => k(Ok((Json::Object(vec![]), rest))),
            _ => kv_pair(
                rest,
                Box::new(|res| match res {
                    Err(e) => k(Err(e)),
                    Ok((elem, tokens)) => object_loop(vec![elem], tokens, k),
                }),
            ),
        },
        _ => k(Err(("unexpected token; expected JSON value", tokens))),
    }
}

fn array_loop<T: 'static>(
    mut elems: Vec<Json>,
    tokens: &[Token],
    k: Cont<Json, T>,
) -> PResult<'_, T> {
    match tokens {
        [Token::RSquare, rest @ ..] => k(Ok((Json::Array(elems), rest))),
        [Token::Comma, rest @ ..] => value(
            rest,
            Box::new(|res| match res {
                Err(e) => k(Err(e)),
                Ok((elem, tokens)) => {
                    elems.push(elem);
                    array_loop(elems, tokens, k)
                }
            }),
        ),
        [_, ..] => k(Err(("unexpected token; expected `]` or `,`", tokens))),
        [] => k(Err(("unexpected EOF; expected `]` or `,`", tokens))),
    }
}

#[allow(unused_variables)] // rust-analyzer has a false-positive
fn kv_pair<T: 'static>(tokens: &[Token], k: Cont<(String, Json), T>) -> PResult<'_, T> {
    let (key, mut tokens) = match tokens {
        [Token::String(key), tokens1 @ ..] => (key.clone(), tokens1),
        [_, ..] => return Err(("unexpected token; expected string or `}`", tokens)),
        [] => return Err(("unexpected EOF; expected string or `}`", tokens)),
    };

    match tokens {
        [Token::Colon, tokens2 @ ..] => tokens = tokens2,
        [_, ..] => return Err(("unexpected token; expected `:`", tokens)),
        [] => return Err(("unexpected EOF; expected `:`", tokens)),
    }

    value(
        tokens,
        Box::new(|res| match res {
            Err(e) => k(PResult::<(String, Json)>::Err(e)),
            Ok((value, tokens)) => {
                let pair = (key, value);
                k(Ok((pair, tokens)))
            }
        }),
    )
}

fn object_loop<T: 'static>(
    mut elems: Vec<(String, Json)>,
    tokens: &[Token],
    k: Cont<Json, T>,
) -> PResult<'_, T> {
    match tokens {
        [Token::RCurly, rest @ ..] => k(Ok((Json::Object(elems), rest))),
        [Token::Comma, rest @ ..] => kv_pair(
            rest,
            Box::new(|res| match res {
                Err(e) => k(Err(e)),
                Ok((elem, tokens)) => {
                    elems.push(elem);
                    object_loop(elems, tokens, k)
                }
            }),
        ),
        [_, ..] => k(Err(("unexpected token; expected `]` or `,`", tokens))),
        [] => k(Err(("unexpected EOF; expected `]` or `,`", tokens))),
    }
}

pub fn parse(tokens: &[Token]) -> PResult<'_, Json> { value(tokens, Box::new(|x| x)) }

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
    }
}
