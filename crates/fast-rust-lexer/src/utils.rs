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
