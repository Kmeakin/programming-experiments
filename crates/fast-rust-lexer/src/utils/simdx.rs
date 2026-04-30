use std::arch::aarch64::{
    uint8x16_t, vandq_u8, vget_lane_u64, vgetq_lane_u16, vgetq_lane_u32, vld1q_u8, vld4q_u8,
    vreinterpret_u64_u8, vreinterpretq_u16_u8, vreinterpretq_u32_u8, vshrn_n_u16,
};
use std::ptr;
use std::simd::prelude::*;

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
fn movemask8(mask: Mask<i8, 8>) -> u64 { mask.to_bitmask() }

#[inline]
fn movemask16(mask: &Mask<i8, 16>) -> u64 {
    unsafe {
        let v0 = vandq_u8(vld1q_u8(ptr::from_ref(mask).cast()), POWERS_OF_2.into());
        let t0 = vpaddq_u8(v0, v0);
        let t1 = vpaddq_u8(t0, t0);
        let t2 = vpaddq_u8(t1, t1);
        u64::from(vgetq_lane_u16(vreinterpretq_u16_u8(t2), 0))
    }
}

#[inline]
fn movemask32(mask: &Mask<i8, 32>) -> u64 {
    unsafe {
        let v0 = vandq_u8(vld1q_u8(ptr::from_ref(mask).cast()), POWERS_OF_2.into());
        let v1 = vandq_u8(
            vld1q_u8(ptr::from_ref(mask).cast::<u8>().add(16)),
            POWERS_OF_2.into(),
        );

        let t0 = vpaddq_u8(v0, v1);
        let t1 = vpaddq_u8(t0, t0);
        let t2 = vpaddq_u8(t1, t1);
        u64::from(vgetq_lane_u32(vreinterpretq_u32_u8(t2), 0))
    }
}

#[inline]
pub fn movemask<const N: usize>(mask: Mask<i8, N>) -> u64 {
    match N {
        8 => movemask8(mask.resize::<8>(false)),
        16 => movemask16(&mask.resize::<16>(false)),
        32 => movemask32(&mask.resize::<32>(false)),
        64 => movemask64(&mask.resize::<64>(false)),
        _ => panic!("Unsupported vector length"),
    }
}

#[inline]
pub fn movemask64(mask: &Mask<i8, 64>) -> u64 {
    unsafe {
        let tuple = { vld4q_u8(ptr::from_ref(mask).cast()) };

        let v0 = tuple.0; // ḾḾḾḾḾḾḾḾ ḬḬḬḬḬḬḬḬ ḚḚḚḚḚḚḚḚ ḀḀḀḀḀḀḀḀ ḿḿḿḿḿḿḿḿ ḭḭḭḭḭḭḭḭ ḛḛḛḛḛḛḛḛ ḁḁḁḁḁḁḁḁ MMMMMMMM IIIIIIII EEEEEEEE AAAAAAAA mmmmmmmm iiiiiiii eeeeeeee aaaaaaaa
        let v1 = tuple.1; // ṈṈṈṈṈṈṈṈ ĴĴĴĴĴĴĴĴ ḞḞḞḞḞḞḞḞ ḄḄḄḄḄḄḄḄ ṉṉṉṉṉṉṉṉ ĵĵĵĵĵĵĵĵ ḟḟḟḟḟḟḟḟ ḅḅḅḅḅḅḅḅ NNNNNNNN JJJJJJJJ FFFFFFFF BBBBBBBB nnnnnnnn jjjjjjjj ffffffff bbbbbbbb
        let v2 = tuple.2; // ṎṎṎṎṎṎṎṎ ḴḴḴḴḴḴḴḴ ḠḠḠḠḠḠḠḠ ḈḈḈḈḈḈḈḈ ṏṏṏṏṏṏṏṏ ḵḵḵḵḵḵḵḵ ḡḡḡḡḡḡḡḡ ḉḉḉḉḉḉḉḉ OOOOOOOO KKKKKKKK GGGGGGGG CCCCCCCC oooooooo kkkkkkkk gggggggg cccccccc
        let v3 = tuple.3; // ṔṔṔṔṔṔṔṔ ḺḺḺḺḺḺḺḺ ḪḪḪḪḪḪḪḪ ḎḎḎḎḎḎḎḎ ṕṕṕṕṕṕṕṕ ḻḻḻḻḻḻḻḻ ḫḫḫḫḫḫḫḫ ḏḏḏḏḏḏḏḏ PPPPPPPP LLLLLLLL HHHHHHHH DDDDDDDD pppppppp llllllll hhhhhhhh dddddddd

        //   ṈṈṈṈṈṈṈṈ ĴĴĴĴĴĴĴĴ ḞḞḞḞḞḞḞḞ ḄḄḄḄḄḄḄḄ ṉṉṉṉṉṉṉṉ ĵĵĵĵĵĵĵĵ ḟḟḟḟḟḟḟḟ ḅḅḅḅḅḅḅḅ NNNNNNNN JJJJJJJJ FFFFFFFF BBBBBBBB nnnnnnnn jjjjjjjj ffffffff bbbbbbbb
        // | ḾḾḾḾḾḾḾḾ ḬḬḬḬḬḬḬḬ ḚḚḚḚḚḚḚḚ ḀḀḀḀḀḀḀḀ ḿḿḿḿḿḿḿḿ ḭḭḭḭḭḭḭḭ ḛḛḛḛḛḛḛḛ ḁḁḁḁḁḁḁḁ MMMMMMMM IIIIIIII EEEEEEEE AAAAAAAA mmmmmmmm iiiiiiii eeeeeeee aaaaaaaa >> 1
        // = ṈḾḾḾḾḾḾḾ ĴḬḬḬḬḬḬḬ ḞḚḚḚḚḚḚḚ ḄḀḀḀḀḀḀḀ ṉḿḿḿḿḿḿḿ ĵḭḭḭḭḭḭḭ ḟḛḛḛḛḛḛḛ ḅḁḁḁḁḁḁḁ NMMMMMMM JIIIIIII FEEEEEEE BAAAAAAA nmmmmmmm jiiiiiii feeeeeee baaaaaaa
        let t0 = vsriq_n_u8(v1, v0, 1);

        //   ṔṔṔṔṔṔṔṔ ḺḺḺḺḺḺḺḺ ḪḪḪḪḪḪḪḪ ḎḎḎḎḎḎḎḎ ṕṕṕṕṕṕṕṕ ḻḻḻḻḻḻḻḻ ḫḫḫḫḫḫḫḫ ḏḏḏḏḏḏḏḏ PPPPPPPP LLLLLLLL HHHHHHHH DDDDDDDD pppppppp llllllll hhhhhhhh dddddddd
        // | ṎṎṎṎṎṎṎṎ ḴḴḴḴḴḴḴḴ ḠḠḠḠḠḠḠḠ ḈḈḈḈḈḈḈḈ ṏṏṏṏṏṏṏṏ ḵḵḵḵḵḵḵḵ ḡḡḡḡḡḡḡḡ ḉḉḉḉḉḉḉḉ OOOOOOOO KKKKKKKK GGGGGGGG CCCCCCCC oooooooo kkkkkkkk gggggggg cccccccc >> 1
        // = ṔṎṎṎṎṎṎṎ ḺḴḴḴḴḴḴḴ ḪḠḠḠḠḠḠḠ ḎḈḈḈḈḈḈḈ ṕṏṏṏṏṏṏṏ ḻḵḵḵḵḵḵḵ ḫḡḡḡḡḡḡḡ ḏḉḉḉḉḉḉḉ POOOOOOO LKKKKKKK HGGGGGGG DCCCCCCC pooooooo lkkkkkkk hggggggg dccccccc
        let t1 = vsriq_n_u8(v3, v2, 1);

        //   ṔṎṎṎṎṎṎṎ ḺḴḴḴḴḴḴḴ ḪḠḠḠḠḠḠḠ ḎḈḈḈḈḈḈḈ ṕṏṏṏṏṏṏṏ ḻḵḵḵḵḵḵḵ ḫḡḡḡḡḡḡḡ ḏḉḉḉḉḉḉḉ POOOOOOO LKKKKKKK HGGGGGGG DCCCCCCC pooooooo lkkkkkkk hggggggg dccccccc
        //   ṈḾḾḾḾḾḾḾ ĴḬḬḬḬḬḬḬ ḞḚḚḚḚḚḚḚ ḄḀḀḀḀḀḀḀ ṉḿḿḿḿḿḿḿ ĵḭḭḭḭḭḭḭ ḟḛḛḛḛḛḛḛ ḅḁḁḁḁḁḁḁ NMMMMMMM JIIIIIII FEEEEEEE BAAAAAAA nmmmmmmm jiiiiiii feeeeeee baaaaaaa >> 2
        // = ṔṎṈḾḾḾḾḾ ḺḴĴḬḬḬḬḬ ḪḠḞḚḚḚḚḚ ḎḈḄḀḀḀḀḀ ṕṏṉḿḿḿḿḿ ḻḵĵḭḭḭḭḭ ḫḡḟḛḛḛḛḛ ḏḉḅḁḁḁḁḁ PONMMMMM LKJIIIII HGFEEEEE DCBAAAAA ponmmmmm lkjiiiii hgfeeeee dcbaaaaa
        let t2 = vsriq_n_u8(t1, t0, 2);

        //   ṔṎṈḾḾḾḾḾ ḺḴĴḬḬḬḬḬ ḪḠḞḚḚḚḚḚ ḎḈḄḀḀḀḀḀ ṕṏṉḿḿḿḿḿ ḻḵĵḭḭḭḭḭ ḫḡḟḛḛḛḛḛ ḏḉḅḁḁḁḁḁ PONMMMMM LKJIIIII HGFEEEEE DCBAAAAA ponmmmmm lkjiiiii hgfeeeee dcbaaaaa
        // | ṔṎṈḾḾḾḾḾ ḺḴĴḬḬḬḬḬ ḪḠḞḚḚḚḚḚ ḎḈḄḀḀḀḀḀ ṕṏṉḿḿḿḿḿ ḻḵĵḭḭḭḭḭ ḫḡḟḛḛḛḛḛ ḏḉḅḁḁḁḁḁ PONMMMMM LKJIIIII HGFEEEEE DCBAAAAA ponmmmmm lkjiiiii hgfeeeee dcbaaaaa >> 4
        // = ṔṎṈḾṔṎṈḾ ḺḴĴḬḺḴĴḬ ḪḠḞḚḪḠḞḚ ḎḈḄḀḎḈḄḀ ṕṏṉḿṕṏṉḿ ḻḵĵḭḻḵĵḭ ḫḡḟḛḫḡḟḛ ḏḉḅḁḏḉḅḁ PONMPONM LKJILKJI HGFEHGFE DCBADCBA ponmponm lkjilkji hgfehgfe dcbadcba
        let t3 = vsriq_n_u8(t2, t2, 4);

        //   trunc(ṔṎṈḾṔṎṈḾḺḴĴḬḺḴĴḬ ḪḠḞḚḪḠḞḚḎḈḄḀḎḈḄḀ ṕṏṉḿṕṏṉḿḻḵĵḭḻḵĵḭ ḫḡḟḛḫḡḟḛḏḉḅḁḏḉḅḁ PONMPONMLKJILKJI HGFEHGFEDCBADCBA ponmponmlkjilkji hgfehgfedcbadcba >> 4)
        // = ṔṎṈḾḺḴĴḬ ḪḠḞḚḎḈḄḀ ṕṏṉḿḻḵĵḭ ḫḡḟḛḏḉḅḁ PONMLKJI HGFEDCBA ponmlkji hgfedcba
        let t4 = vshrn_n_u16(vreinterpretq_u16_u8(t3), 4);

        vget_lane_u64(vreinterpret_u64_u8(t4), 0)
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
