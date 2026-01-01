use crate::Json;
use crate::Token;

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
                    Token::True => state = State::ValueDone(Json::Bool(true)),
                    Token::False => state = State::ValueDone(Json::Bool(false)),
                    Token::Null => state = State::ValueDone(Json::Null),
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
