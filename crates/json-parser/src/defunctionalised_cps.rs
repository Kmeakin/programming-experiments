use crate::{Json, Token};

type ErrCont<'tokens, Ret> = fn((&'static str, &'tokens [Token])) -> Ret;

enum ValueFrame {
    ArrayLoop {},
    ArrayPush { elems: Vec<Json> },
    KVPair { key: String },
}

enum KVPairFrame {
    ObjectLoop {},
    ObjectPush { elems: Vec<(String, Json)> },
}

struct Cont<F> {
    ret:     F,
    value:   Vec<ValueFrame>,
    kv_pair: Vec<KVPairFrame>,
}

impl<F> Cont<F> {
    fn push_value(&mut self, frame: ValueFrame) -> &mut Self {
        self.value.push(frame);
        self
    }

    fn push_kv_pair(&mut self, frame: KVPairFrame) -> &mut Self {
        self.kv_pair.push(frame);
        self
    }
}

fn apply_value<'tokens, F, T>(
    ok: &mut Cont<F>,
    err: ErrCont<'tokens, T>,
    value: Json,
    tokens: &'tokens [Token],
) -> T
where
    F: FnMut(Json, &'tokens [Token]) -> T,
{
    let Some(frame) = ok.value.pop() else {
        return (ok.ret)(value, tokens);
    };

    match frame {
        ValueFrame::ArrayLoop {} => array_loop(vec![value], tokens, ok, err),
        ValueFrame::ArrayPush { mut elems } => {
            elems.push(value);
            array_loop(elems, tokens, ok, err)
        }
        ValueFrame::KVPair { key } => apply_kv_pair(ok, err, key, value, tokens),
    }
}

fn apply_kv_pair<'tokens, F, T>(
    ok: &mut Cont<F>,
    err: ErrCont<'tokens, T>,
    key: String,
    value: Json,
    tokens: &'tokens [Token],
) -> T
where
    F: FnMut(Json, &'tokens [Token]) -> T,
{
    let Some(frame) = ok.kv_pair.pop() else {
        unreachable!()
    };

    match frame {
        KVPairFrame::ObjectLoop {} => object_loop(vec![(key, value)], tokens, ok, err),
        KVPairFrame::ObjectPush { mut elems } => {
            elems.push((key, value));
            object_loop(elems, tokens, ok, err)
        }
    }
}

fn value<'tokens, F, T>(tokens: &'tokens [Token], ok: &mut Cont<F>, err: ErrCont<'tokens, T>) -> T
where F: FnMut(Json, &'tokens [Token]) -> T {
    let [token, rest @ ..] = tokens else {
        return err(("unexpected EOF; expected JSON value", tokens));
    };

    match token {
        Token::KwTrue => apply_value(ok, err, Json::Bool(true), rest),
        Token::KwFalse => apply_value(ok, err, Json::Bool(false), rest),
        Token::KwNull => apply_value(ok, err, Json::Null, rest),
        Token::Number => apply_value(ok, err, Json::Number, rest),
        Token::String(s) => apply_value(ok, err, Json::String(s.clone()), rest),
        Token::LSquare => match rest {
            [] => err(("unexpected EOF; unterminated `[`", rest)),
            [Token::RSquare, rest @ ..] => apply_value(ok, err, Json::Array(vec![]), rest),
            _ => value(rest, ok.push_value(ValueFrame::ArrayLoop {}), err),
        },
        Token::LCurly => match rest {
            [] => err(("unexpected EOF; unterminated `{`", rest)),
            [Token::RCurly, rest @ ..] => apply_value(ok, err, Json::Object(vec![]), rest),
            _ => kv_pair(rest, ok.push_kv_pair(KVPairFrame::ObjectLoop {}), err),
        },
        _ => err(("unexpected token; expected JSON value", tokens)),
    }
}

fn array_loop<'tokens, F, T>(
    elems: Vec<Json>,
    tokens: &'tokens [Token],
    ok: &mut Cont<F>,
    err: ErrCont<'tokens, T>,
) -> T
where
    F: FnMut(Json, &'tokens [Token]) -> T,
{
    match tokens {
        [Token::RSquare, rest @ ..] => apply_value(ok, err, Json::Array(elems), rest),
        [Token::Comma, rest @ ..] => {
            value(rest, ok.push_value(ValueFrame::ArrayPush { elems }), err)
        }
        [_, ..] => err(("unexpected token; expected `]` or `,`", tokens)),
        [] => err(("unexpected EOF; expected `]` or `,`", tokens)),
    }
}

fn kv_pair<'tokens, F, T>(
    tokens: &'tokens [Token],
    ok: &mut Cont<F>,
    err: ErrCont<'tokens, T>,
) -> T
where
    F: FnMut(Json, &'tokens [Token]) -> T,
{
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

    value(tokens, ok.push_value(ValueFrame::KVPair { key }), err)
}

fn object_loop<'tokens, F, T>(
    elems: Vec<(String, Json)>,
    tokens: &'tokens [Token],
    ok: &mut Cont<F>,
    err: ErrCont<'tokens, T>,
) -> T
where
    F: FnMut(Json, &'tokens [Token]) -> T,
{
    match tokens {
        [Token::RCurly, rest @ ..] => apply_value(ok, err, Json::Object(elems), rest),
        [Token::Comma, rest @ ..] => kv_pair(
            rest,
            ok.push_kv_pair(KVPairFrame::ObjectPush { elems }),
            err,
        ),
        [_, ..] => err(("unexpected token; expected `]` or `,`", tokens)),
        [] => err(("unexpected EOF; expected `]` or `,`", tokens)),
    }
}

pub fn parse(tokens: &[Token]) -> Result<(Json, &[Token]), (&'static str, &[Token])> {
    value(
        tokens,
        &mut Cont {
            ret:     |value, tokens| Ok((value, tokens)),
            value:   vec![],
            kv_pair: vec![],
        },
        Err,
    )
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
