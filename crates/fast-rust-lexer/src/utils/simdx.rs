#![allow(clippy::wildcard_imports)]

use std::arch::aarch64::*;
use std::simd::prelude::*;

pub fn eq<const N: usize>(vec: Simd<u8, N>, byte: u8) -> Mask<i8, N> {
    vec.simd_eq(Simd::splat(byte))
}

pub fn in_range<const N: usize>(vec: Simd<u8, N>, min: u8, max: u8) -> Mask<i8, N> {
    Simd::splat(min).simd_le(vec) & vec.simd_le(Simd::splat(max))
}

#[must_use]
#[inline]
pub unsafe fn load<const VEC_LEN: usize>(ptr: *const u8) -> Simd<u8, VEC_LEN> {
    unsafe {
        match VEC_LEN {
            #[cfg(false)]
            16 => ptr.cast::<Simd<u8, 16>>().read_unaligned().resize(0),
            16 => std::mem::transmute::<uint16x4x2_t, Simd<u8, 16>>(vld2_u16(ptr.cast::<u16>()))
                .resize(0),

            #[cfg(false)]
            32 => ptr.cast::<Simd<u8, 32>>().read_unaligned().resize(0),
            32 => std::mem::transmute::<uint16x8x2_t, Simd<u8, 32>>(vld2q_u16(ptr.cast::<u16>()))
                .resize(0),

            #[cfg(false)]
            64 => ptr.cast::<Simd<u8, 64>>().read_unaligned().resize(0),
            64 => std::mem::transmute::<uint8x16x4_t, Simd<u8, 64>>(vld4q_u8(ptr)).resize(0),
            _ => unreachable!(),
        }
    }
}

#[rustfmt::skip]
const POWERS_OF_2: Simd<u8, 16> = Simd::from_array([
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
]);

#[allow(improper_ctypes)]
unsafe extern "C" {
    #[link_name = "llvm.aarch64.neon.addp.v16i8"]
    pub fn vpaddq_u8(a: uint8x16_t, b: uint8x16_t) -> uint8x16_t;

    #[link_name = "llvm.aarch64.neon.vsri.v16i8"]
    pub fn vsriq_n_u8(a: uint8x16_t, b: uint8x16_t, c: u32) -> uint8x16_t;

    #[link_name = "llvm.aarch64.neon.ld4.v16i8"]
    pub fn vld4q_u8(ptr: *const u8) -> uint8x16x4_t;
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
pub fn movemask_interleaved16(mask: Mask<i8, 16>) -> u16 {
    unsafe {
        let uint16x4x2_t(v0, v1) = std::mem::transmute::<Mask<i8, 16>, uint16x4x2_t>(mask);
        let t0 = vshrn_n_u16(vcombine_u16(v0, vdup_n_u16(0)), 7);
        let t1 = vshrn_n_u16(vcombine_u16(v1, vdup_n_u16(0)), 7);
        let t2 = vsli_n_u8(t0, t1, 2);
        let t3 = vsli_n_u8(t2, t2, 4);
        let t4 = vcombine_u8(t3, vdup_n_u8(0));
        let t5 = vreinterpretq_u16_u8(t4);
        let t6 = vshrn_n_u16(t5, 4);
        vget_lane_u16(vreinterpret_u16_u8(t6), 0)
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

/*
const uint16x8x2_t chunk = vld2q_u16((const uint16_t*)(const void*)src);
const uint8x16_t chunk0 = vreinterpretq_u8_u16(chunk.val[0]);
const uint8x16_t chunk1 = vreinterpretq_u8_u16(chunk.val[1]);
const uint8x16_t dup = vdupq_n_u8(tag);
const uint8x16_t cmp0 = vceqq_u8(chunk0, dup);
const uint8x16_t cmp1 = vceqq_u8(chunk1, dup);
const uint8x8_t t0 = vshrn_n_u16(vreinterpretq_u16_u8(cmp0), 6);
const uint8x8_t t1 = vshrn_n_u16(vreinterpretq_u16_u8(cmp1), 6);
const uint8x8_t res = vsli_n_u8(t0, t1, 4);
return vget_lane_u64(vreinterpret_u64_u8(res), 0);
// Optional AND with 0xaaaaaaaaaaaaaaaa for iterations

const uint16x8x2_t chunk = vld2q_u16((const uint16_t*)(const void*)src);


const uint8x8_t t0 = vshrn_n_u16(vreinterpretq_u16_u8(v0), 6);
trunc(DDDDDDDDCCCCCCCC zzzzzzzzyyyyyyyy vvvvvvvvuuuuuuuu rrrrrrrrqqqqqqqq nnnnnnnnmmmmmmmm jjjjjjjjiiiiiiii ffffffffeeeeeeee bbbbbbbbaaaaaaaa >> 7)
t0 =  DDDDDDDC zzzzzzzy vvvvvvvu rrrrrrrq nnnnnnnm jjjjjjji fffffffe bbbbbbba

const uint8x8_t t1 = vshrn_n_u16(vreinterpretq_u16_u8(v1), 6);
trunc(FFFFFFFFEEEEEEEE BBBBBBBBAAAAAAAA xxxxxxxxwwwwwwww ttttttttssssssss ppppppppoooooooo llllllllkkkkkkkk hhhhhhhhgggggggg ddddddddcccccccc >> 7)
t1 =  FFFFFFFE BBBBBBBA xxxxxxxw ttttttts pppppppo lllllllk hhhhhhhg dddddddc

    DDDDDDDC zzzzzzzy vvvvvvvu rrrrrrrq nnnnnnnm jjjjjjji fffffffe bbbbbbba
  | FFFFFFFE BBBBBBBA xxxxxxxw ttttttts pppppppo lllllllk hhhhhhhg dddddddc << 2
  = FFFFFEDC BBBBBAzy xxxxxwvu tttttsrq ppppponm lllllkji hhhhhgfe dddddcba
const t2 = vsli_n_u8(t0, t1, 2);


    FFFFFEDC BBBBBAzy xxxxxwvu tttttsrq ppppponm lllllkji hhhhhgfe dddddcba
  | FFFFFEDC BBBBBAzy xxxxxwvu tttttsrq ppppponm lllllkji hhhhhgfe dddddcba << 4
  = FEDCFEDC BAzyBAzy xwvuxwvu tsrqtsrq ponmponm lkjilkji hgfehgfe dcbadcba
const t3 = vsli_n_u8(t2, t2, 4);


  trunc(FEDCFEDCBAzyBAzy xwvuxwvutsrqtsrq ponmponmlkjilkji hgfehgfedcbadcba >> 4)
        FEDCBAzyxwvutsrqponmlkjihgfedcba
const t4 = vshrn_n_u16(vreinterpretq_u16_u8(t3), 4);
*/

#[inline]
pub fn movemask_interleaved32(mask: Mask<i8, 32>) -> u32 {
    unsafe {
        let uint16x8x2_t(v0, v1) = std::mem::transmute::<Mask<i8, 32>, uint16x8x2_t>(mask);
        let t0 = vshrn_n_u16(v0, 7);
        let t1 = vshrn_n_u16(v1, 7);
        let t2 = vsli_n_u8(t0, t1, 2);
        let t3 = vsli_n_u8(t2, t2, 4);
        let t4 = vcombine_u8(t3, t3);
        let t5 = vreinterpretq_u16_u8(t4);
        let t6 = vshrn_n_u16(t5, 4);
        vget_lane_u32(vreinterpret_u32_u8(t6), 0)
    }
}

#[inline]
pub fn movemask_interleaved64(mask: Mask<i8, 64>) -> u64 {
    unsafe {
        let uint8x16x4_t(v0, v1, v2, v3) = std::mem::transmute::<Mask<i8, 64>, uint8x16x4_t>(mask);
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

        #[cfg(false)]
        16 => u64::from(movemask16(mask.resize::<16>(false))),
        16 => u64::from(movemask_interleaved16(mask.resize::<16>(false))),

        #[cfg(false)]
        32 => u64::from(movemask32(mask.resize::<32>(false))),
        32 => u64::from(movemask_interleaved32(mask.resize::<32>(false))),

        #[cfg(false)]
        64 => movemask64(mask.resize::<64>(false)),
        64 => movemask_interleaved64(mask.resize::<64>(false)),
        _ => panic!("Unsupported vector length"),
    }
}

#[inline]
pub fn first_set<const N: usize>(mask: Mask<i8, N>) -> Option<usize> {
    let mask = movemask(mask);
    if mask == 0 {
        None
    } else {
        Some(mask.trailing_zeros() as usize)
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
        assert_eq!(u64::from(movemask16(mask)), mask.to_bitmask());
    }
}

#[test]
fn check_movemask32() {
    for i in 0..32 {
        let mask: Mask<i8, 32> = Mask::from_bitmask(1 << i);
        assert_eq!(u64::from(movemask32(mask)), mask.to_bitmask());
    }
}

#[test]
fn check_movemask64() {
    for i in 0..64 {
        let mask: Mask<i8, 64> = Mask::from_bitmask(1 << i);
        assert_eq!(movemask64(mask), mask.to_bitmask());
    }
}

#[test]
fn check_movemask_interleaved16() {
    unsafe {
        for i in 0..16 {
            let mask: Mask<i8, 16> = Mask::from_bitmask(1 << i);
            let neon = vld2_u16((&raw const mask).cast::<u16>());
            let rust = std::mem::transmute::<uint16x4x2_t, Mask<i8, 16>>(neon);
            assert_eq!(u64::from(movemask_interleaved16(rust)), mask.to_bitmask());
        }
    }
}

#[test]
fn check_movemask_interleaved32() {
    unsafe {
        for i in 0..32 {
            let mask: Mask<i8, 32> = Mask::from_bitmask(1 << i);
            let neon = vld2q_u16((&raw const mask).cast::<u16>());
            let rust = std::mem::transmute::<uint16x8x2_t, Mask<i8, 32>>(neon);
            assert_eq!(u64::from(movemask_interleaved32(rust)), mask.to_bitmask());
        }
    }
}

#[test]
fn check_movemask_interleaved64() {
    unsafe {
        for i in 0..64 {
            let mask: Mask<i8, 64> = Mask::from_bitmask(1 << i);
            let neon = vld4q_u8((&raw const mask).cast::<u8>());
            let rust = std::mem::transmute::<uint8x16x4_t, Mask<i8, 64>>(neon);
            assert_eq!(movemask_interleaved64(rust), mask.to_bitmask());
        }
    }
}
