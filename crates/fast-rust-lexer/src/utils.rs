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
