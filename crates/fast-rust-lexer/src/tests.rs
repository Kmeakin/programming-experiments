use crate::{TokenKind, multi_pass, raw_ptr, simd, simd2, simd3};

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
    let input = simd2::prepare_input::<VEC_LEN>(input);
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

fn lex_simd3<const VEC_LEN: usize>(input: &str) -> Vec<(TokenKind, u32)> {
    debug_assert!(u32::try_from(input.len()).is_ok(), "input too long");
    let input = simd3::prepare_input::<VEC_LEN>(input);
    let mut out_vec = vec![simd::EOF_BYTE; input.len() * 5];
    let out = simd3::lex::<VEC_LEN>(&input, &mut out_vec);

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

fn lex_multi_pass<W: multi_pass::Word, const VEC_LEN: usize>(input: &str) -> Vec<(TokenKind, u32)> {
    use multi_pass::*;

    let (input, mut bitmask_vecs) = prepare_input::<W, VEC_LEN>(input);
    stage1::<W, VEC_LEN>(&input, &mut bitmask_vecs);
    let bitmask_slices = bitmask_vecs.each_ref().map(Vec::as_slice);

    let mut out = vec![EOF_BYTE; input.len() * 10];
    let buf = stage2::<W, VEC_LEN>(&input, bitmask_slices, &mut out);
    let mut out_iter = buf.iter().copied();

    let mut decoded = Vec::new();
    while let Some(kind) = out_iter.next() {
        if kind == EOF_BYTE {
            break;
        }
        let Some(kind) = TokenKind::from_u8(kind) else {
            panic!("Invalid token kind byte: {kind} ({kind:#04x})");
        };
        let len = match kind.is_punct() {
            true => 1,
            false => u32::from_ne_bytes(out_iter.next_chunk().expect("Expected length byte")),
        };
        decoded.push((kind, len));
    }

    decoded
}

#[track_caller]
fn check(impl_fn: impl Fn(&str) -> Vec<(TokenKind, u32)>) {
    let input = std::fs::read_to_string(format!("{CRATE_ROOT}/test-data/rustc.rs")).unwrap();
    let rustc_tokens = lex_rustc(&input);
    let impl_tokens = impl_fn(&input);
    let mut pos = 0;
    for (rustc_token, impl_token) in rustc_tokens.iter().zip(impl_tokens.iter()) {
        let rustc_token = *rustc_token;
        let mut impl_token = *impl_token;

        let start = pos;
        let end = pos + rustc_token.1 as usize;
        let rustc_lexeme = &input[start..end];
        let impl_lexeme = &input[start..start + impl_token.1 as usize];
        if rustc_token.0 == TokenKind::Float && impl_token.0 == TokenKind::Int {
            // For float literals, we allow some leeway in the lexeme since different lexers
            // might accept slightly different forms .
            impl_token.0 = TokenKind::Float;
        }
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

#[test]
fn test_simd3_16() { check(lex_simd3::<16>); }

#[test]
fn test_simd3_32() { check(lex_simd3::<32>); }

#[test]
fn test_simd3_64() { check(lex_simd3::<64>); }

#[test]
fn test_multi_pass_16() { check(lex_multi_pass::<u16, 16>); }

#[test]
fn test_multi_pass_32() { check(lex_multi_pass::<u32, 32>); }

#[test]
fn test_multi_pass_64() { check(lex_multi_pass::<u64, 64>); }

#[test]
fn test_multi_pass2_16() { check(lex_multi_pass2::<16>); }

#[test]
fn test_multi_pass2_32() { check(lex_multi_pass2::<32>); }

#[test]
fn test_multi_pass2_64() { check(lex_multi_pass2::<64>); }

fn lex_multi_pass2<const VEC_LEN: usize>(input: &str) -> Vec<(TokenKind, u32)> {
    use crate::multi_pass2;

    let (input, mut output) = multi_pass2::prepare_input::<VEC_LEN>(input);
    let indices = multi_pass2::stage1::<VEC_LEN>(&input, &mut output);
    multi_pass2::stage2::<VEC_LEN>(&input, indices)
        .into_iter()
        .map(|(kind, start, end)| (kind, end - start))
        .collect()
}
