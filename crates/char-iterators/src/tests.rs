// There are only 1,114,112 code points (including surrogates for WTF-8). So we
// can test `next_code_point` and `next_code_point_reverse` exhaustively on all
// possible inputs.

/// Assert that encoding a codepoint with `encode_utf8_raw` and then decoding it
/// with `next_code_point` preserves the codepoint.
fn test_next_code_point(codepoint: u32) {
    let mut bytes = [0; 4];
    let mut bytes = std::char::encode_utf8_raw(codepoint, &mut bytes).iter();

    // SAFETY: `bytes` is UTF8-like
    let got = unsafe { crate::after::next_code_point(&mut bytes) };
    assert_eq!(got, Some(codepoint));

    // SAFETY: `bytes` is UTF8-like
    let got = unsafe { crate::after::next_code_point(&mut bytes) };
    assert_eq!(got, None);
}

/// The same but for `next_code_point_reverse`.
fn test_next_code_point_reverse(codepoint: u32) {
    let mut bytes = [0; 4];
    let mut bytes = std::char::encode_utf8_raw(codepoint, &mut bytes).iter();

    // SAFETY: `bytes` is UTF8-like
    let got = unsafe { crate::after::next_code_point_reverse(&mut bytes) };
    assert_eq!(got, Some(codepoint));

    // SAFETY: `bytes` is UTF8-like
    let got = unsafe { crate::after::next_code_point_reverse(&mut bytes) };
    assert_eq!(got, None);
}

#[test]
#[cfg_attr(miri, ignore)] // Disabled on Miri because it is too slow
fn test_next_code_point_exhaustive() {
    for c in 0..=u32::from(char::MAX) {
        test_next_code_point(c);
    }
}

#[test]
#[cfg_attr(miri, ignore)] // Disabled on Miri because it is too slow
fn test_next_code_point_reverse_exhaustive() {
    for c in 0..=u32::from(char::MAX) {
        test_next_code_point_reverse(c);
    }
}

#[rustfmt::skip]
const CODEPOINT_BOUNDARIES: &[u32] = &[
    // 1 byte codepoints (U+0000 ..= U+007F):
    0x0000, 0x007F,

    // 2 byte codepoints (U+0080 ..= U+07FF):
    0x0080, 0x07FF,

    // 3 byte codepoints (U+0800 ..= U+FFFF):
    0x0800, 0xFFFF,

    // 4 byte codepoints (U+01_0000 ..= U+10_FFFF):
    0x01_0000, 0x10_FFFF,
];

#[test]
fn test_next_code_point_boundary_conditions() {
    for c in CODEPOINT_BOUNDARIES {
        test_next_code_point(*c);
    }
}

#[test]
fn test_next_code_point_reverse_boundary_conditions() {
    for c in CODEPOINT_BOUNDARIES {
        test_next_code_point_reverse(*c);
    }
}
