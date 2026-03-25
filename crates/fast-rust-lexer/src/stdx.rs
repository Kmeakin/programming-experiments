#[inline]
pub unsafe fn push_unchecked<T>(vec: &mut Vec<T>, value: T) {
    debug_assert!(vec.len() < vec.capacity());
    unsafe {
        vec.as_mut_ptr().add(vec.len()).write(value);
        vec.set_len(vec.len() + 1);
    }
}
