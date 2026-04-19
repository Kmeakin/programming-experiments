use std::simd::prelude::*;

#[rustfmt::skip]
const POWERS_OF_2: Simd<u8, 16> = Simd::from_array([
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
]);

#[inline]
fn deinterleave_add(a: Simd<u8, 16>, b: Simd<u8, 16>) -> Simd<u8, 16> {
    #[allow(improper_ctypes)]
    unsafe extern "C" {
        #[link_name = "llvm.aarch64.neon.addp.v16i8"]
        fn pairwise_add(a: Simd<u8, 16>, b: Simd<u8, 16>) -> Simd<u8, 16>;
    }
    unsafe { pairwise_add(a, b) }
}

#[inline]
fn movemask8(mask: Mask<i8, 8>) -> u64 { mask.to_bitmask() }

#[inline]
fn movemask16(mask: Mask<i8, 16>) -> u64 {
    let v0 = mask.extract::<0x00, 16>().to_simd().cast() & POWERS_OF_2;

    let sum0 = deinterleave_add(v0, v0);
    let sum1 = deinterleave_add(sum0, sum0);
    let sum2 = deinterleave_add(sum1, sum1);
    u64::from(Simd::<u16, 8>::from_ne_bytes(sum2.to_ne_bytes())[0])
}

#[inline]
fn movemask32(mask: Mask<i8, 32>) -> u64 {
    let v0 = mask.extract::<0x00, 16>().to_simd().cast() & POWERS_OF_2;
    let v1 = mask.extract::<0x10, 16>().to_simd().cast() & POWERS_OF_2;

    let sum0 = deinterleave_add(v0, v1);
    let sum1 = deinterleave_add(sum0, sum0);
    let sum2 = deinterleave_add(sum1, sum1);
    u64::from(Simd::<u32, 4>::from_ne_bytes(sum2.to_ne_bytes())[0])
}

#[inline]
fn movemask64(mask: Mask<i8, 64>) -> u64 {
    let v0 = mask.extract::<0x00, 16>().to_simd().cast() & POWERS_OF_2;
    let v1 = mask.extract::<0x10, 16>().to_simd().cast() & POWERS_OF_2;
    let v2 = mask.extract::<0x20, 16>().to_simd().cast() & POWERS_OF_2;
    let v3 = mask.extract::<0x30, 16>().to_simd().cast() & POWERS_OF_2;

    let sum0 = deinterleave_add(v0, v1);
    let sum1 = deinterleave_add(v2, v3);
    let sum2 = deinterleave_add(sum0, sum1);
    let sum3 = deinterleave_add(sum2, sum2);
    Simd::<u64, 2>::from_ne_bytes(sum3.to_ne_bytes())[0]
}

#[inline]
pub fn movemask<const N: usize>(mask: Mask<i8, N>) -> u64 {
    match N {
        8 => movemask8(mask.resize::<8>(false)),
        16 => movemask16(mask.resize::<16>(false)),
        32 => movemask32(mask.resize::<32>(false)),
        64 => movemask64(mask.resize::<64>(false)),
        _ => panic!("Unsupported vector length"),
    }
}

#[test]
fn check_movemask8() {
    for i in 0..8 {
        let mask: Mask<i8, 8> = Mask::from_bitmask(1 << i);
        assert_eq!(movemask(mask), mask.to_bitmask());
    }
}

#[test]
fn check_movemask16() {
    for i in 0..16 {
        let mask: Mask<i8, 16> = Mask::from_bitmask(1 << i);
        assert_eq!(movemask(mask), mask.to_bitmask());
    }
}

#[test]
fn check_movemask32() {
    for i in 0..32 {
        let mask: Mask<i8, 32> = Mask::from_bitmask(1 << i);
        assert_eq!(movemask(mask), mask.to_bitmask());
    }
}

#[test]
fn check_movemask64() {
    for i in 0..64 {
        let mask: Mask<i8, 64> = Mask::from_bitmask(1 << i);
        assert_eq!(movemask(mask), mask.to_bitmask());
    }
}
