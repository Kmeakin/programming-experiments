use crate::{Json, Token};

type Cont<'tokens, 'cont, Arg, Ret> = Box<dyn FnOnce((Arg, &'tokens [Token])) -> Ret + 'cont>;
type ErrCont<'tokens, Ret> = fn((&'static str, &'tokens [Token])) -> Ret;

fn value<'tokens, T>(
    tokens: &'tokens [Token],
    ok: Cont<'tokens, '_, Json, T>,
    err: ErrCont<'tokens, T>,
) -> T {
    let [token, rest @ ..] = tokens else {
        return err(("unexpected EOF; expected JSON value", tokens));
    };

    match token {
        Token::True => ok((Json::Bool(true), rest)),
        Token::False => ok((Json::Bool(false), rest)),
        Token::Null => ok((Json::Null, rest)),
        Token::Number => ok((Json::Number, rest)),
        Token::String(s) => ok((Json::String(s.clone()), rest)),
        Token::LSquare => match rest {
            [] => err(("unexpected EOF; unterminated `[`", rest)),
            [Token::RSquare, rest @ ..] => ok((Json::Array(vec![]), rest)),
            _ => value(
                rest,
                Box::new(|(elem, tokens)| array_loop(vec![elem], tokens, ok, err)),
                err,
            ),
        },
        Token::LCurly => match rest {
            [] => err(("unexpected EOF; unterminated `{`", rest)),
            [Token::RCurly, rest @ ..] => ok((Json::Object(vec![]), rest)),
            _ => kv_pair(
                rest,
                Box::new(|(elem, tokens)| object_loop(vec![elem], tokens, ok, err)),
                err,
            ),
        },
        _ => err(("unexpected token; expected JSON value", tokens)),
    }
}

fn array_loop<'tokens, T>(
    mut elems: Vec<Json>,
    tokens: &'tokens [Token],
    ok: Cont<'tokens, '_, Json, T>,
    err: ErrCont<'tokens, T>,
) -> T {
    match tokens {
        [Token::RSquare, rest @ ..] => ok((Json::Array(elems), rest)),
        [Token::Comma, rest @ ..] => value(
            rest,
            Box::new(|(elem, tokens)| {
                elems.push(elem);
                array_loop(elems, tokens, ok, err)
            }),
            err,
        ),
        [_, ..] => err(("unexpected token; expected `]` or `,`", tokens)),
        [] => err(("unexpected EOF; expected `]` or `,`", tokens)),
    }
}

fn kv_pair<'tokens, T>(
    tokens: &'tokens [Token],
    ok: Cont<'tokens, '_, (String, Json), T>,
    err: ErrCont<'tokens, T>,
) -> T {
    let (key, mut tokens) = match tokens {
        [Token::String(key), tokens1 @ ..] => (key.clone(), tokens1),
        [_, ..] => return err(("unexpected token; expected string or `}`", tokens)),
        [] => return err(("unexpected EOF; expected string or `}`", tokens)),
    };

    match tokens {
        [Token::Colon, tokens2 @ ..] => tokens = tokens2,
        [_, ..] => return err(("unexpected token; expected `:`", tokens)),
        [] => return err(("unexpected EOF; expected `:`", tokens)),
    }

    value(
        tokens,
        Box::new(|(value, tokens)| ok(((key, value), tokens))),
        err,
    )
}

fn object_loop<'tokens, T>(
    mut elems: Vec<(String, Json)>,
    tokens: &'tokens [Token],
    ok: Cont<'tokens, '_, Json, T>,
    err: ErrCont<'tokens, T>,
) -> T {
    match tokens {
        [Token::RCurly, rest @ ..] => ok((Json::Object(elems), rest)),
        [Token::Comma, rest @ ..] => kv_pair(
            rest,
            Box::new(|(elem, tokens)| {
                elems.push(elem);
                object_loop(elems, tokens, ok, err)
            }),
            err,
        ),
        [_, ..] => err(("unexpected token; expected `]` or `,`", tokens)),
        [] => err(("unexpected EOF; expected `]` or `,`", tokens)),
    }
}

pub fn parse(tokens: &[Token]) -> Result<(Json, &[Token]), (&'static str, &[Token])> {
    value(tokens, Box::new(Ok), Err)
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
