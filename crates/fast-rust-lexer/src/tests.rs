use crate::{TokenKind, raw_ptr};

const CRATE_ROOT: &str = env!("CARGO_MANIFEST_DIR");

fn lex_rustc(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    crate::rustc::tokenize(input, crate::rustc::FrontmatterAllowed::No)
        .map(|token| (TokenKind::from(token.kind), token.len))
        .collect()
}

fn lex_logos(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    crate::logos::lex_iter(input).collect()
}

fn lex_manual(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    crate::manual::lex_iter(input).collect()
}

fn lex_manual_loop(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let mut tokens = Vec::new();
    crate::manual_loop::lex_loop(input, |kind, len| tokens.push((kind, len)));
    tokens
}

fn lex_jump_threading(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let mut tokens = Vec::new();
    crate::jump_threading::lex_loop(input.as_bytes(), |kind, len| {
        tokens.push((kind, len as u32));
    });
    tokens
}

fn lex_raw_ptr(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let mut input = input.as_bytes().to_vec();
    input.extend([raw_ptr::EOF_BYTE; raw_ptr::EOF_PADDING]);
    let mut tokens = Vec::new();
    raw_ptr::lex_loop(&input, |kind, start, end| {
        let len = unsafe { end.offset_from_unsigned(start) };
        tokens.push((kind, len as u32));
    });
    tokens
}

#[track_caller]
fn check(impl_fn: impl Fn(&str) -> Vec<(TokenKind, u32)>) {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    let rustc_tokens = lex_rustc(&input);
    let impl_tokens = impl_fn(&input);
    for (rustc_token, impl_token) in rustc_tokens.iter().zip(impl_tokens.iter()) {
        assert_eq!(rustc_token, impl_token);
    }
    assert_eq!(rustc_tokens.len(), impl_tokens.len());
}

#[test]
fn test_logos() { check(lex_logos); }

#[test]
fn test_manual() { check(lex_manual); }

#[test]
fn test_manual_loop() { check(lex_manual_loop); }

#[test]
fn test_jump_threading() { check(lex_jump_threading); }

#[test]
fn test_raw_ptr() { check(lex_raw_ptr); }
