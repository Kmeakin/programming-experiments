//! Test lexers against the rustc implementation.

#![allow(clippy::needless_pass_by_value)]

use crate::common::{Lexer, Rustc};

const RUST_AMALGAMATION: &str = include_str!("../test-data/rust.rs");

#[track_caller]
fn check(src: &str, lexer: impl Lexer) {
    let mut tokens = Vec::new();
    Rustc {}.lex_str(src, (), |(), kind, range, lexeme| {
        tokens.push((kind, range, lexeme));
    });
    let mut iter = tokens.into_iter();

    lexer.lex_str(src, (), |(), kind, range, lexeme| {
        let got = (kind, range, lexeme);
        let expected = iter.next().unwrap();
        assert_eq!(expected, got);
    });
    assert_eq!(iter.next(), None);
}

#[track_caller]
pub fn integration_tests(lexer: impl Lexer) { check(RUST_AMALGAMATION, lexer); }
