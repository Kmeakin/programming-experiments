use std::arch::aarch64::{
    uint8x16_t, uint8x16x2_t, uint8x16x4_t, vandq_u8, vget_lane_u64, vgetq_lane_u16,
    vgetq_lane_u32, vgetq_lane_u64, vld4q_u8, vreinterpret_u64_u8, vreinterpretq_u16_u8,
    vreinterpretq_u32_u8, vreinterpretq_u64_u8, vshrn_n_u16,
};
use std::ptr;
use std::simd::prelude::*;

pub fn eq<const N: usize>(vec: Simd<u8, N>, byte: u8) -> Mask<i8, N> {
    vec.simd_eq(Simd::splat(byte))
}

pub fn in_range<const N: usize>(vec: Simd<u8, N>, min: u8, max: u8) -> Mask<i8, N> {
    Simd::splat(min).simd_le(vec) & vec.simd_le(Simd::splat(max))
}

#[rustfmt::skip]
const POWERS_OF_2: Simd<u8, 16> = Simd::from_array([
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
]);

#[allow(improper_ctypes)]
unsafe extern "C" {
    #[link_name = "llvm.aarch64.neon.vsri.v16i8"]
    fn vsriq_n_u8(a: uint8x16_t, b: uint8x16_t, c: u32) -> uint8x16_t;

    #[link_name = "llvm.aarch64.neon.addp.v16i8"]
    fn vpaddq_u8(a: uint8x16_t, b: uint8x16_t) -> uint8x16_t;
}

#[inline]
fn movemask8(mask: Mask<i8, 8>) -> u8 { mask.to_bitmask() as u8 }

#[inline]
fn movemask16(mask: Mask<i8, 16>) -> u16 {
    unsafe {
        let v0 = std::mem::transmute::<Mask<i8, 16>, uint8x16_t>(mask);
        let v0 = vandq_u8(v0, POWERS_OF_2.into());
        let t0 = vpaddq_u8(v0, v0);
        let t1 = vpaddq_u8(t0, t0);
        let t2 = vpaddq_u8(t1, t1);
        vgetq_lane_u16(vreinterpretq_u16_u8(t2), 0)
    }
}

#[inline]
fn movemask32(mask: Mask<i8, 32>) -> u32 {
    unsafe {
        let uint8x16x2_t(v0, v1) = std::mem::transmute::<Mask<i8, 32>, uint8x16x2_t>(mask);
        let v0 = vandq_u8(v0, POWERS_OF_2.into());
        let v1 = vandq_u8(v1, POWERS_OF_2.into());
        let t0 = vpaddq_u8(v0, v1);
        let t1 = vpaddq_u8(t0, t0);
        let t2 = vpaddq_u8(t1, t1);
        vgetq_lane_u32(vreinterpretq_u32_u8(t2), 0)
    }
}

#[inline]
fn movemask64(mask: Mask<i8, 64>) -> u64 {
    unsafe {
        let uint8x16x4_t(v0, v1, v2, v3) = std::mem::transmute::<Mask<i8, 64>, uint8x16x4_t>(mask);
        let v0 = vandq_u8(v0, POWERS_OF_2.into());
        let v1 = vandq_u8(v1, POWERS_OF_2.into());
        let v2 = vandq_u8(v2, POWERS_OF_2.into());
        let v3 = vandq_u8(v3, POWERS_OF_2.into());
        let t0 = vpaddq_u8(v0, v1);
        let t1 = vpaddq_u8(v2, v3);
        let t2 = vpaddq_u8(t0, t1);
        let t3 = vpaddq_u8(t2, t2);
        vgetq_lane_u64(vreinterpretq_u64_u8(t3), 0)
    }
}

#[inline]
pub fn movemask_interleaved64(mask: &Mask<i8, 64>) -> u64 {
    unsafe {
        let uint8x16x4_t(v0, v1, v2, v3) = vld4q_u8(ptr::from_ref(mask).cast());
        let t0 = vsriq_n_u8(v1, v0, 1);
        let t1 = vsriq_n_u8(v3, v2, 1);
        let t2 = vsriq_n_u8(t1, t0, 2);
        let t3 = vsriq_n_u8(t2, t2, 4);
        let t4 = vshrn_n_u16(vreinterpretq_u16_u8(t3), 4);
        vget_lane_u64(vreinterpret_u64_u8(t4), 0)
    }
}

/// `Mask::to_bitmask` is suboptimal on `AArch64`.
#[inline]
pub fn movemask<const N: usize>(mask: Mask<i8, N>) -> u64 {
    match N {
        8 => u64::from(movemask8(mask.resize::<8>(false))),
        16 => u64::from(movemask16(mask.resize::<16>(false))),
        32 => u64::from(movemask32(mask.resize::<32>(false))),
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

#[test]
fn check_movemask_interleaved64() {
    for i in 0..64 {
        let mask: Mask<i8, 64> = Mask::from_bitmask(1 << i);
        assert_eq!(movemask_interleaved64(&mask), mask.to_bitmask());
    }
}
