pub use std::simd::prelude;
use std::simd::prelude::*;

pub fn first_set(mask: Mask<i8, 16>) -> Option<usize> {
    // If bitmasks are efficient, using them is better
    if cfg!(target_feature = "sse") {
        return mask.first_set();
    }

    let iota = Simd::from_array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
    let min = (!mask.to_simd() | iota).cast::<u8>().reduce_min();
    if min == u8::MAX {
        return None;
    }
    Some(min as usize)
}
