#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Expr<'expr> {
    Int(u32),
    Var(usize),
    Fun { body: &'expr Self },
    App { fun: &'expr Self, arg: &'expr Self },
    Let { val: &'expr Self, body: &'expr Self },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'expr> {
    Int(u32),
    Fun {
        env: Vec<Value<'expr>>,
        body: Expr<'expr>,
    },
}

pub type Env<'expr> = Vec<Value<'expr>>;

pub fn lookup<'expr, 'slice>(env: &'slice [Value<'expr>], index: usize) -> &'slice Value<'expr> {
    let Some(value) = env.iter().nth_back(index) else {
        panic!(
            "Unbound local variable (tried to lookup index {index} in an environment of length {})",
            env.len()
        )
    };
    value
}
