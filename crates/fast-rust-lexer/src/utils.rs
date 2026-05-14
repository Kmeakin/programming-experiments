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
pub unsafe fn write_and_advance<T>(out: *mut u8, val: T) -> *mut u8 {
    unsafe {
        out.cast::<T>().write_unaligned(val);
        out.add(size_of::<T>())
    }
}

#[must_use]
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
        write_and_advance(out, len)
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
