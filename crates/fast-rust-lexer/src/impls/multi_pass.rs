#![allow(clippy::wildcard_imports)]

use std::ops::BitOr;
use std::simd::prelude::*;

use crate::utils::simdx::*;

pub const EOF_BYTE: u8 = 0xFF;
const NUM_VECS: usize = 5;

pub fn prepare_input<W: Word, const VEC_LEN: usize>(src: &str) -> (Vec<u8>, [Vec<W>; NUM_VECS]) {
    unsafe {
        let size = src.len() + VEC_LEN * 2;
        let layout = std::alloc::Layout::from_size_align(size, VEC_LEN).unwrap();
        let ptr = std::alloc::alloc(layout);
        assert!(!ptr.is_null());
        let mut input_vec = Vec::from_raw_parts(ptr, 0, size);
        input_vec.extend(src.as_bytes());
        input_vec.extend([EOF_BYTE; VEC_LEN]);
        input_vec.extend([EOF_BYTE; VEC_LEN]);
        assert!(input_vec.as_ptr().is_aligned_to(VEC_LEN));

        let num_words = input_vec.len().div_ceil(VEC_LEN);
        let out_vecs = [
            vec![W::ZERO; num_words],
            vec![W::ZERO; num_words],
            vec![W::ZERO; num_words],
            vec![W::ZERO; num_words],
            vec![W::ZERO; num_words],
        ];
        (input_vec, out_vecs)
    }
}

pub trait Word: Copy {
    const ZERO: Self;

    type Vec: Copy;
    type Mask: Copy + BitOr<Output = Self::Mask>;

    /// # Safety
    /// Usual pointer validity rules.
    unsafe fn load(ptr: *const u8) -> Self::Vec;
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask;
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask;
    fn movemask(mask: Self::Mask) -> Self;
}

impl Word for u16 {
    const ZERO: Self = 0;

    type Vec = Simd<u8, 16>;
    type Mask = Mask<i8, 16>;

    unsafe fn load(ptr: *const u8) -> Self::Vec { unsafe { load::<16>(ptr) } }
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask { vec.simd_eq(Simd::splat(byte)) }
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask {
        Simd::splat(start).simd_le(vec) & vec.simd_le(Simd::splat(end))
    }
    fn movemask(mask: Self::Mask) -> Self { movemask(mask) as Self }
}

impl Word for u32 {
    const ZERO: Self = 0;

    type Vec = Simd<u8, 32>;
    type Mask = Mask<i8, 32>;

    unsafe fn load(ptr: *const u8) -> Self::Vec { unsafe { load::<32>(ptr) } }
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask { vec.simd_eq(Simd::splat(byte)) }
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask {
        Simd::splat(start).simd_le(vec) & vec.simd_le(Simd::splat(end))
    }
    fn movemask(mask: Self::Mask) -> Self { movemask(mask) as Self }
}

impl Word for u64 {
    const ZERO: Self = 0;

    type Vec = Simd<u8, 64>;
    type Mask = Mask<i8, 64>;

    unsafe fn load(ptr: *const u8) -> Self::Vec { unsafe { load::<64>(ptr) } }
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask { vec.simd_eq(Simd::splat(byte)) }
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask {
        Simd::splat(start).simd_le(vec) & vec.simd_le(Simd::splat(end))
    }
    fn movemask(mask: Self::Mask) -> Self { movemask(mask) }
}

pub fn stage1_16(src: &[u8], out: &mut [Vec<u16>; NUM_VECS]) { stage1::<u16, 16>(src, out) }
pub fn stage1_32(src: &[u8], out: &mut [Vec<u32>; NUM_VECS]) { stage1::<u32, 32>(src, out) }
pub fn stage1_64(src: &[u8], out: &mut [Vec<u64>; NUM_VECS]) { stage1::<u64, 64>(src, out) }

pub fn stage1<W: Word, const VEC_LEN: usize>(src: &[u8], out: &mut [Vec<W>; NUM_VECS]) {
    const { assert!(VEC_LEN == size_of::<W>() * 8) }

    debug_assert_eq!(src.last_chunk(), Some(&[EOF_BYTE; VEC_LEN]));

    let mut ptr = src.as_ptr();
    let src_end = src.as_ptr_range().end;

    let [
        newline_vec,
        double_quote_vec,
        single_quote_vec,
        digits_vec,
        ident_vec,
    ] = out;
    let mut newline_ptr = newline_vec.as_mut_ptr();
    let mut double_quote_ptr = double_quote_vec.as_mut_ptr();
    let mut single_quote_ptr = single_quote_vec.as_mut_ptr();
    let mut digits_ptr = digits_vec.as_mut_ptr();
    let mut ident_ptrs = ident_vec.as_mut_ptr();

    unsafe {
        while ptr < src_end {
            let vec = W::load(ptr);
            let newlines = W::movemask(W::eq(vec, b'\n'));
            let double_quotes = W::movemask(W::eq(vec, b'"'));
            let single_quotes = W::movemask(W::eq(vec, b'\''));
            let digits = W::movemask(W::eq(vec, b'_') | W::in_range(vec, b'0', b'9'));
            let idents = W::movemask(W::in_range(vec, b'a', b'z') | W::in_range(vec, b'A', b'Z'));

            newline_ptr.write(newlines);
            double_quote_ptr.write(double_quotes);
            single_quote_ptr.write(single_quotes);
            digits_ptr.write(digits);
            ident_ptrs.write(idents);

            ptr = ptr.add(VEC_LEN);
            newline_ptr = newline_ptr.add(1);
            double_quote_ptr = double_quote_ptr.add(1);
            single_quote_ptr = single_quote_ptr.add(1);
            digits_ptr = digits_ptr.add(1);
            ident_ptrs = ident_ptrs.add(1);
        }
    }
}
