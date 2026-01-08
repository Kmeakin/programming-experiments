use crate::syntax::{Expr, Value, lookup};

fn push<T>(mut v: Vec<T>, x: T) -> Vec<T> {
    v.push(x);
    v
}

enum State<'a> {
    /// Descending down into an expression.
    Expr(Expr<'a>),
    /// Ascending up from a value.
    Value(Value<'a>),
}

enum Frame<'a> {
    /// After you've finished evaluating the function, remember to evaluate the
    /// argument
    AppArg { arg: Expr<'a> },
    /// After you've finished evaluating the argument, remember to apply the
    /// function to the argument
    AppFun { fun: Value<'a> },
    /// After you've finished evaluating the function body, remember to restore
    /// the previous environment.
    AppRestore { env: Vec<Value<'a>> },

    /// After you've finished evaluating the let initializer, remember to
    /// evaluate the body
    LetBody { body: Expr<'a> },
    /// After you've finished evaluating the let body, remember to pop the
    /// let-bound variable off the environment.
    LetPop {},
}

fn step<'a>(state: State<'a>, env: &mut Vec<Value<'a>>, stack: &mut Vec<Frame<'a>>) -> State<'a> {
    match state {
        State::Expr(expr) => match expr {
            Expr::Int(n) => State::Value(Value::Int(n)),
            Expr::Var(index) => State::Value(lookup(env, index).clone()),
            Expr::Fun { body } => State::Value(Value::Fun {
                env: env.clone(),
                body: *body,
            }),
            Expr::App { fun, arg } => {
                stack.push(Frame::AppArg { arg: *arg });
                State::Expr(*fun)
            }
            Expr::Let { val, body } => {
                stack.push(Frame::LetBody { body: *body });
                State::Expr(*val)
            }
        },
        State::Value(value) => match stack.pop() {
            // Done
            None => State::Value(value),
            Some(frame) => match frame {
                Frame::AppArg { arg } => {
                    stack.push(Frame::AppFun { fun: value });
                    State::Expr(arg)
                }
                Frame::AppFun { fun } => {
                    let Value::Fun { env: new_env, body } = fun else {
                        panic!(
                            "Bad function application (tried to apply `{fun:?}`, which is not a \
                             function"
                        )
                    };

                    stack.push(Frame::AppRestore {
                        env: std::mem::replace(env, push(new_env, value)),
                    });
                    State::Expr(body)
                }
                Frame::AppRestore { env: new_env } => {
                    *env = new_env;
                    State::Value(value)
                }

                Frame::LetBody { body } => {
                    stack.push(Frame::LetPop {});
                    env.push(value);
                    State::Expr(body)
                }
                Frame::LetPop {} => {
                    env.pop();
                    State::Value(value)
                }
            },
        },
    }
}

pub fn eval<'a>(expr: Expr<'a>, mut env: Vec<Value<'a>>) -> Value<'a> {
    let mut state = State::Expr(expr);
    let mut stack = Vec::new();
    loop {
        match step(state, &mut env, &mut stack) {
            State::Value(value) if stack.is_empty() => return value,
            new_state => state = new_state,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::Env;

    use super::*;

    #[track_caller]
    #[allow(clippy::needless_pass_by_value)]
    fn assert_eval<'expr>(env: Env<'expr>, expr: Expr<'expr>, expect: Value<'expr>) {
        let got = eval(expr, env);
        assert_eq!(got, expect);
    }

    #[test]
    fn test_eval() {
        // eval(42, []) == 42
        assert_eval(vec![], Expr::Int(42), Value::Int(42));

        // eval(v0, [42]) == 42
        assert_eval(vec![Value::Int(42)], Expr::Var(0), Value::Int(42));

        // eval(let x = 42 in x, []) == 42
        assert_eval(
            vec![],
            Expr::Let {
                val: &Expr::Int(42),
                body: &Expr::Var(0),
            },
            Value::Int(42),
        );

        // eval(fun x => x, []) == fun x => x
        assert_eval(
            vec![],
            Expr::Fun {
                body: &Expr::Var(0),
            },
            Value::Fun {
                env: vec![],
                body: Expr::Var(0),
            },
        );

        // eval((fun x => x) 42, []) == 42
        assert_eval(
            vec![],
            Expr::App {
                fun: &Expr::Fun {
                    body: &Expr::Var(0),
                },
                arg: &Expr::Int(42),
            },
            Value::Int(42),
        );

        // eval((fun x y => x) 24 42, []) == 24
        assert_eval(
            vec![],
            Expr::App {
                fun: &Expr::App {
                    fun: &Expr::Fun {
                        body: &Expr::Fun {
                            body: &Expr::Var(1),
                        },
                    },
                    arg: &Expr::Int(24),
                },
                arg: &Expr::Int(42),
            },
            Value::Int(24),
        );

        // eval(let f = fun x => x in f 42, []) == 42
        assert_eval(
            vec![],
            Expr::Let {
                val: &Expr::Fun {
                    body: &Expr::Var(0),
                },
                body: &Expr::App {
                    fun: &Expr::Var(0),
                    arg: &Expr::Int(42),
                },
            },
            Value::Int(42),
        );
    }
}
