/// # Safety
/// Same as `memchr::arch::all::memchr::One::find_raw`
#[inline]
pub unsafe fn memchr_raw(needle: u8, start: *const u8, end: *const u8) -> Option<*const u8> {
    unsafe { memchr::arch::all::memchr::One::new(needle).find_raw(start, end) }
}
