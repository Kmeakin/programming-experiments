use crate::{Json, Token};

pub enum State {
    ValueStart,
    ValueDone(Json),
}

pub enum Frame {
    Array1(Vec<Json>),
}

pub fn parse(mut tokens: &[Token]) -> Result<(Json, &[Token]), (&'static str, &[Token])> {
    let mut stack: Vec<Frame> = Vec::new();
    let mut state = State::ValueStart;

    loop {
        match state {
            State::ValueStart => {
                let [token, rest @ ..] = tokens else {
                    return Err(("unexpected EOF; expected JSON value", tokens));
                };
                match token {
                    Token::KwTrue => state = State::ValueDone(Json::Bool(true)),
                    Token::KwFalse => state = State::ValueDone(Json::Bool(false)),
                    Token::KwNull => state = State::ValueDone(Json::Null),
                    Token::Number => state = State::ValueDone(Json::Number),
                    Token::String(s) => state = State::ValueDone(Json::String(s.clone())),
                    Token::LSquare => match rest {
                        [] => return Err(("unexpected EOF; unterminated `[`", rest)),
                        [Token::RSquare, rest @ ..] => {
                            tokens = rest;
                            state = State::ValueDone(Json::Array(vec![]));
                            continue;
                        }
                        [_, ..] => {
                            stack.push(Frame::Array1(vec![]));
                            state = State::ValueStart;
                        }
                    },
                    Token::RSquare
                    | Token::LCurly
                    | Token::RCurly
                    | Token::Comma
                    | Token::Colon => {
                        return Err(("unexpected token; expected JSON value", tokens));
                    }
                }
                tokens = rest;
            }
            State::ValueDone(value) => match stack.pop() {
                None => return Ok((value, tokens)),
                Some(frame) => match frame {
                    Frame::Array1(mut elems) => {
                        elems.push(value);
                        match tokens {
                            [Token::RSquare, rest @ ..] => {
                                tokens = rest;
                                state = State::ValueDone(Json::Array(elems));
                            }
                            [Token::Comma, rest @ ..] => {
                                tokens = rest;
                                stack.push(Frame::Array1(elems));
                                state = State::ValueStart;
                            }
                            [] => return Err(("unexpected EOF; expected `]` or `,`", tokens)),
                            _ => return Err(("unexpected token; expected `]` or `,`", tokens)),
                        }
                    }
                },
            },
        }
    }
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
