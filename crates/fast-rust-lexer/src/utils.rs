use crate::TokenKind;

pub mod bitstring;
pub mod simdx;
pub mod tbl;

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

/// `ptr::write_unaligned` combined with `ptr::add` to advance the pointer.
///
/// # Safety
///
/// Inherits the safety requirements of `ptr::write_unaligned` and `ptr::add`.
#[inline]
pub unsafe fn write_and_advance<T>(out: *mut T, val: T) -> *mut T {
    unsafe {
        out.write_unaligned(val);
        out.add(1)
    }
}

#[must_use]
#[track_caller]
pub unsafe fn write_token(
    out: *mut u8,
    kind: TokenKind,
    start: *const u8,
    end: *const u8,
) -> *mut u8 {
    unsafe {
        let len = end.offset_from_unsigned(start) as u32;
        debug_assert_ne!(len, 0);
        let out = write_and_advance(out, kind as u8);
        write_and_advance(out.cast(), len).cast()
    }
}

#[must_use]
pub unsafe fn write_punct(out: *mut u8, kind: u8) -> *mut u8 {
    unsafe { write_and_advance(out, kind) }
}

#[inline]
pub fn is_punct(b: u8) -> bool {
    #[allow(clippy::match_like_matches_macro)]
    match b {
        | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-' | b'*'
        | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.' | b'!' | b'>'
        | b'<' | b'^' => true,
        _ => false,
    }
}

#[inline]
pub fn align_down<const ALIGN: usize>(ptr: *const u8) -> *const u8 {
    const { assert!(ALIGN.is_power_of_two()) }
    ptr.map_addr(|addr| addr & !(ALIGN - 1))
}

#[macro_export]
macro_rules! unroll {
    (0, $block:block) => {};
    (1, $block:block) => {
        $block
    };
    (2, $block:block) => {
        $block;
        unroll!(1, $block)
    };
    (3, $block:block) => {
        $block;
        unroll!(2, $block)
    };
    (4, $block:block) => {
        $block;
        unroll!(3, $block)
    };
    (5, $block:block) => {
        $block;
        unroll!(4, $block)
    };
    (6, $block:block) => {
        $block;
        unroll!(5, $block)
    };
    (7, $block:block) => {
        $block;
        unroll!(6, $block)
    };
    (8, $block:block) => {
        $block;
        unroll!(7, $block)
    };
    (16, $block:block) => {
        unroll!(8, $block);
        unroll!(8, $block)
    };
    (32, $block:block) => {
        unroll!(16, $block);
        unroll!(16, $block)
    };
}
pub use crate::unroll;

/// Like `eprintln`, but only when `debug_assertions` are enabled.
#[macro_export]
macro_rules! deprintln {
    ($($args:tt)*) => {
        if cfg!(debug_assertions) {
            eprintln!($($args)*);
        }
    };
}
pub use crate::deprintln;
