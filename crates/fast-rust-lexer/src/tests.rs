use crate::{TokenKind, raw_ptr, simd, simd2};

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

fn lex_raw_ptr<const VEC_LEN: usize>(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let mut input = input.as_bytes().to_vec();
    input.extend([raw_ptr::EOF_BYTE; VEC_LEN]);
    let mut tokens = Vec::new();
    raw_ptr::lex_loop::<VEC_LEN, _>(&input, |kind, start, end| {
        let len = unsafe { end.offset_from_unsigned(start) };
        tokens.push((kind, len as u32));
    });
    tokens
}

fn lex_simd<const VEC_LEN: usize>(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let mut input = input.as_bytes().to_vec();
    input.extend([simd::EOF_BYTE; VEC_LEN]);
    input.extend([simd::EOF_BYTE; VEC_LEN]);
    let mut out_vec = vec![simd::EOF_BYTE; input.len() * 5];
    let out = simd::lex::<VEC_LEN>(&input, &mut out_vec);

    let mut tokens = Vec::new();
    let mut iter = out.iter().copied();
    while let Some(byte) = iter.next() {
        if byte == simd::EOF_BYTE {
            break;
        }
        let Some(kind) = TokenKind::from_u8(byte) else {
            panic!("Invalid token kind byte: {byte} ({byte:#04x})");
        };
        let len = match kind.is_punct() {
            true => 1,
            false => u32::from_ne_bytes(iter.next_chunk().expect("Expected length byte")),
        };
        tokens.push((kind, len));
    }
    tokens
}

fn lex_simd2<const VEC_LEN: usize>(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let mut input = input.as_bytes().to_vec();
    input.extend([simd::EOF_BYTE; VEC_LEN]);
    input.extend([simd::EOF_BYTE; VEC_LEN]);
    let mut out_vec = vec![simd::EOF_BYTE; input.len() * 5];
    let out = simd2::lex::<VEC_LEN>(&input, &mut out_vec);

    let mut tokens = Vec::new();
    let mut iter = out.iter().copied();
    while let Some(byte) = iter.next() {
        if byte == simd::EOF_BYTE {
            break;
        }
        let Some(kind) = TokenKind::from_u8(byte) else {
            panic!("Invalid token kind byte: {byte} ({byte:#04x})");
        };
        let len = match kind.is_punct() {
            true => 1,
            false => u32::from_ne_bytes(iter.next_chunk().expect("Expected length byte")),
        };
        tokens.push((kind, len));
    }
    tokens
}

#[track_caller]
fn check(impl_fn: impl Fn(&str) -> Vec<(TokenKind, u32)>) {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    let rustc_tokens = lex_rustc(&input);
    let impl_tokens = impl_fn(&input);
    let mut pos = 0;
    for (rustc_token, impl_token) in rustc_tokens.iter().zip(impl_tokens.iter()) {
        let start = pos;
        let end = pos + rustc_token.1 as usize;
        let rustc_lexeme = &input[start..end];
        let impl_lexeme = &input[start..start + impl_token.1 as usize];
        assert_eq!(
            rustc_token, impl_token,
            "Token mismatch at byte position {start}: expected {:?} (lexeme: {:?}), got {:?} \
             (lexeme: {:?})",
            rustc_token.0, rustc_lexeme, impl_token.0, impl_lexeme
        );
        pos = end;
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
fn test_raw_ptr_16() { check(lex_raw_ptr::<16>); }

#[test]
fn test_raw_ptr_32() { check(lex_raw_ptr::<32>); }

#[test]
fn test_raw_ptr_64() { check(lex_raw_ptr::<64>); }

#[test]
fn test_simd_16() { check(lex_simd::<16>); }

#[test]
fn test_simd_32() { check(lex_simd::<32>); }

#[test]
fn test_simd_64() { check(lex_simd::<64>); }

#[test]
fn test_simd2_16() { check(lex_simd2::<16>); }

#[test]
fn test_simd2_32() { check(lex_simd2::<32>); }

#[test]
fn test_simd2_64() { check(lex_simd2::<64>); }
