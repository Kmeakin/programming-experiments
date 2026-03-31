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
    crate::logos::lex_iter(input)
        .map(|(kind, len)| (TokenKind::from(kind), len))
        .collect()
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

fn check_logos(input: &str) {
    let rustc_tokens = lex_rustc(input);
    let logos_tokens = lex_logos(input);

    let mut rustc_pos = 0;
    let mut logos_pos = 0;
    for (rustc_token @ (_, rustc_len), logos_token @ (_, logos_len)) in
        rustc_tokens.iter().zip(&logos_tokens)
    {
        let rustc_src = &input[rustc_pos..rustc_pos + *rustc_len as usize];
        let logos_src = &input[logos_pos..logos_pos + *logos_len as usize];
        assert_eq!(
            rustc_token, logos_token,
            "\nrustc_src: `{rustc_src}`\nlogos_src: `{logos_src}`"
        );
        rustc_pos += *rustc_len as usize;
        logos_pos += *logos_len as usize;
    }
}

fn check_manual(input: &str) {
    let rustc_tokens = lex_rustc(input);
    let manual_tokens = lex_manual(input);

    let mut rustc_pos = 0;
    let mut manual_pos = 0;
    for (rustc_token @ (_, rustc_len), manual_token @ (_, manual_len)) in
        rustc_tokens.iter().zip(&manual_tokens)
    {
        let rustc_src = &input[rustc_pos..rustc_pos + *rustc_len as usize];
        let manual_src = &input[manual_pos..manual_pos + *manual_len as usize];
        assert_eq!(
            rustc_token, manual_token,
            "\nrustc_src: `{rustc_src}`\nmanual_src: `{manual_src}`"
        );
        rustc_pos += *rustc_len as usize;
        manual_pos += *manual_len as usize;
    }
}

fn check_manual_loop(input: &str) {
    let rustc_tokens = lex_rustc(input);
    let manual_tokens = lex_manual_loop(input);

    let mut rustc_pos = 0;
    let mut manual_pos = 0;
    for (rustc_token @ (_, rustc_len), manual_token @ (_, manual_len)) in
        rustc_tokens.iter().zip(&manual_tokens)
    {
        let rustc_src = &input[rustc_pos..rustc_pos + *rustc_len as usize];
        let manual_src = &input[manual_pos..manual_pos + *manual_len as usize];
        assert_eq!(
            rustc_token, manual_token,
            "\nrustc_src: `{rustc_src}`\nmanual_src: `{manual_src}`"
        );
        rustc_pos += *rustc_len as usize;
        manual_pos += *manual_len as usize;
    }
}

fn check_jump_threading(input: &str) {
    let rustc_tokens = lex_rustc(input);
    let jump_threading_tokens = lex_jump_threading(input);

    let mut rustc_pos = 0;
    let mut manual_pos = 0;
    for (rustc_token @ (_, rustc_len), manual_token @ (_, manual_len)) in
        rustc_tokens.iter().zip(&jump_threading_tokens)
    {
        let rustc_src = &input[rustc_pos..rustc_pos + *rustc_len as usize];
        let manual_src = &input[manual_pos..manual_pos + *manual_len as usize];
        assert_eq!(
            rustc_token, manual_token,
            "\nrustc_src: `{rustc_src}`\nmanual_src: `{manual_src}`"
        );
        rustc_pos += *rustc_len as usize;
        manual_pos += *manual_len as usize;
    }
}

fn check_raw_ptr(input: &str) {
    let rustc_tokens = lex_rustc(input);
    let raw_ptr_tokens = lex_raw_ptr(input);

    let mut rustc_pos = 0;
    let mut manual_pos = 0;
    for (rustc_token @ (_, rustc_len), manual_token @ (_, manual_len)) in
        rustc_tokens.iter().zip(&raw_ptr_tokens)
    {
        let rustc_src = &input[rustc_pos..rustc_pos + *rustc_len as usize];
        let manual_src = &input[manual_pos..manual_pos + *manual_len as usize];
        assert_eq!(
            rustc_token, manual_token,
            "\nrustc_src: `{rustc_src}`\nmanual_src: `{manual_src}`"
        );
        rustc_pos += *rustc_len as usize;
        manual_pos += *manual_len as usize;
    }
}

#[test]
fn test_logos() {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    check_logos(&input);
}

#[test]
fn test_manual() {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    check_manual(&input);
}

#[test]
fn test_manual_loop() {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    check_manual_loop(&input);
}

#[test]
fn test_jump_threading() {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    check_jump_threading(&input);
}

#[test]
fn test_raw_ptr() {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    check_raw_ptr(&input);
}
