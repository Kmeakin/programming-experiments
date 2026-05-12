use std::simd::prelude::*;

pub fn eq<const N: usize>(vec: Simd<u8, N>, byte: u8) -> Mask<i8, N> {
    vec.simd_eq(Simd::splat(byte))
}

pub fn in_range<const N: usize>(vec: Simd<u8, N>, min: u8, max: u8) -> Mask<i8, N> {
    Simd::splat(min).simd_le(vec) & vec.simd_le(Simd::splat(max))
}

#[must_use]
#[inline]
/// # Safety
/// Usual ptr read requirements.
pub unsafe fn load<const VEC_LEN: usize>(ptr: *const u8) -> Simd<u8, VEC_LEN> {
    unsafe {
        debug_assert!(ptr.is_aligned_to(VEC_LEN));
        cfg_select! {
            target_arch = "aarch64" => aarch64::load(ptr),
            target_arch = "x86_64" => ptr.cast::<Simd<u8, VEC_LEN>>().read()
        }
    }
}

#[must_use]
#[inline]
pub fn movemask<const N: usize>(mask: Mask<i8, N>) -> u64 {
    cfg_select! {
        target_arch = "aarch64" => aarch64::movemask(mask),
        target_arch = "x86_64" => mask.to_bitmask()
    }
}

#[must_use]
#[inline]
pub fn first_set<const N: usize>(mask: Mask<i8, N>) -> Option<usize> {
    let mask = movemask(mask);
    if mask == 0 {
        None
    } else {
        Some(mask.trailing_zeros() as usize)
    }
}

#[cfg(target_arch = "aarch64")]
#[allow(clippy::wildcard_imports)]
mod aarch64 {
    use std::arch::aarch64::*;
    use std::mem::transmute;
    use std::simd::prelude::*;

    #[allow(improper_ctypes)]
    unsafe extern "C" {
        #[link_name = "llvm.aarch64.neon.vsri.v16i8"]
        pub fn vsriq_n_u8(a: uint8x16_t, b: uint8x16_t, c: u32) -> uint8x16_t;

        #[link_name = "llvm.aarch64.neon.ld4.v16i8"]
        pub fn vld4q_u8(ptr: *const u8) -> uint8x16x4_t;
    }

    #[must_use]
    #[inline]
    #[allow(clippy::cast_ptr_alignment)]
    pub unsafe fn load<const VEC_LEN: usize>(ptr: *const u8) -> Simd<u8, VEC_LEN> {
        unsafe {
            match VEC_LEN {
                16 => transmute::<uint16x4x2_t, u8x16>(vld2_u16(ptr.cast::<u16>())).resize(0),
                32 => transmute::<uint16x8x2_t, u8x32>(vld2q_u16(ptr.cast::<u16>())).resize(0),
                64 => transmute::<uint8x16x4_t, u8x64>(vld4q_u8(ptr.cast::<u8>())).resize(0),
                _ => unreachable!(),
            }
        }
    }

    #[inline]
    pub fn movemask_interleaved16(mask: Mask<i8, 16>) -> u16 {
        unsafe {
            let uint16x4x2_t(v0, v1) = transmute::<Mask<i8, 16>, uint16x4x2_t>(mask);
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
    pub fn movemask_interleaved32(mask: Mask<i8, 32>) -> u32 {
        unsafe {
            let uint16x8x2_t(v0, v1) = transmute::<Mask<i8, 32>, uint16x8x2_t>(mask);
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
            let uint8x16x4_t(v0, v1, v2, v3) = transmute::<Mask<i8, 64>, uint8x16x4_t>(mask);
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
            16 => u64::from(movemask_interleaved16(mask.resize::<16>(false))),
            32 => u64::from(movemask_interleaved32(mask.resize::<32>(false))),
            64 => movemask_interleaved64(mask.resize::<64>(false)),
            _ => panic!("Unsupported vector length"),
        }
    }

    #[test]
    fn check_movemask_interleaved16() {
        unsafe {
            for i in 0..16 {
                let mask: Mask<i8, 16> = Mask::from_bitmask(1 << i);
                let neon = vld2_u16((&raw const mask).cast::<u16>());
                let rust = transmute::<uint16x4x2_t, Mask<i8, 16>>(neon);
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
                let rust = transmute::<uint16x8x2_t, Mask<i8, 32>>(neon);
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
                let rust = transmute::<uint8x16x4_t, Mask<i8, 64>>(neon);
                assert_eq!(movemask_interleaved64(rust), mask.to_bitmask());
            }
        }
    }
}
