use crate::syntax::{Env, Expr, Value, lookup};

pub fn eval<'expr>(expr: &Expr<'expr>, env: &mut Env<'expr>) -> Value<'expr> {
    match expr {
        Expr::Int(val) => Value::Int(*val),
        Expr::Var(index) => {
            let val = lookup(env, *index);
            val.clone()
        }

        Expr::Fun { body } => Value::Fun {
            env: env.clone(),
            body: **body,
        },
        Expr::App { fun, arg } => {
            let fun = eval(fun, env);
            let arg = eval(arg, env);
            let Value::Fun {
                env: mut new_env,
                body,
            } = fun
            else {
                panic!(
                    "Bad function application (tried to apply `{fun:?}`, which is not a function)"
                )
            };
            new_env.push(arg);
            eval(&body, &mut new_env)
        }

        Expr::Let { val, body } => {
            let val = eval(val, env);
            env.push(val);
            let body = eval(body, env);
            env.pop();
            body
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn assert_eval<'expr>(mut env: Env<'expr>, expr: Expr<'expr>, expect: Value<'expr>) {
        let got = eval(&expr, &mut env);
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
