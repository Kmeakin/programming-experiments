#![cfg(false)] // WIP

use std::simd::prelude::*;

fn is_alphanumeric(byte: u8) -> bool {
    matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

fn is_alphanumeric_vec(vec: Simd<u8, 16>) -> Mask<i8, 16> {
    (vec.simd_eq(Simd::splat(b'_')))
        | (Simd::splat(b'a').simd_le(vec) & vec.simd_le(Simd::splat(b'z')))
        | (Simd::splat(b'A').simd_le(vec) & vec.simd_le(Simd::splat(b'Z')))
        | (Simd::splat(b'0').simd_le(vec) & vec.simd_le(Simd::splat(b'9')))
}

const LUT: [u8; 16] = {
    let mut lut = [0u8; 16];
    // '_' = 0x5f
    // 'A' = 0x41, 'Z' = 0x5a
    // 'a' = 0x61, 'z' = 0x7a
    // '0' = 0x30, '9' = 0x39
    lut[(b'_' & 0x0f) as usize] = 1u8 << (b'_' >> 4);

    let mut i = b'A';
    while i <= b'Z' {
        lut[(i & 0x0f) as usize] |= 1u8 << (i >> 4);
        i += 1;
    }

    let mut i = b'a';
    while i <= b'z' {
        lut[(i & 0x0f) as usize] |= 1u8 << (i >> 4);
        i += 1;
    }

    let mut i = b'0';
    while i <= b'9' {
        lut[(i & 0x0f) as usize] |= 1u8 << (i >> 4);
        i += 1;
    }
    lut
};

fn is_alphanumeric_tbl(vec: Simd<u8, 16>) -> Mask<i8, 16> {
    let lo_nibble = vec & Simd::splat(0x0f);
    let hi_nibble = vec >> 4;
    let lut = Simd::from_array(LUT);
    let tbl = lut.swizzle_dyn(lo_nibble);
    (tbl & (Simd::splat(1u8) << hi_nibble)).simd_ne(Simd::splat(0))
}

#[cfg(test)]
mod lut_tests {
    use super::*;

    fn check(byte: u8) {
        let vec = Simd::splat(byte);
        assert_eq!(
            is_alphanumeric_vec(vec),
            is_alphanumeric_tbl(vec),
            "byte: {byte:#04x}"
        );
    }

    #[test]
    fn test_lut() {
        for b in 0..0x80 {
            check(b);
        }
        check(0xff);
    }
}
