pub mod tbl;

use std::simd::prelude::*;

/// Optimized version of `std::simd::Mask::first_set`.
pub fn first_set<const N: usize>(mask: Mask<i8, N>) -> Option<usize> {
    // If bitmasks are efficient, using them is better
    if cfg!(target_feature = "sse") {
        return mask.first_set();
    }

    let iota: Simd<i8, N> = const {
        let mut array = [0i8; N];
        let mut i = 0;
        while i < N {
            array[i] = i as i8;
            i += 1;
        }
        Simd::from_array(array)
    };
    let min = (!mask.to_simd() | iota).cast::<u8>().reduce_min();
    if min == u8::MAX {
        return None;
    }
    Some(min as usize)
}

/// Like `Vec::push`, but without bounds checking.
///
/// # Safety
///
/// The caller must ensure that `vec.len() < vec.capacity()`.
#[inline]
pub unsafe fn push_unchecked<T>(vec: &mut Vec<T>, value: T) {
    debug_assert!(vec.len() < vec.capacity());
    unsafe {
        vec.as_mut_ptr().add(vec.len()).write(value);
        vec.set_len(vec.len() + 1);
    }
}
