use std::hint::assert_unchecked;

/// Checks whether the byte is a UTF-8 continuation byte (i.e., starts with the
/// bits `10`).
#[inline]
pub const fn utf8_is_cont_byte(byte: u8) -> bool { (byte as i8) < -64 }

/// Reads the next code point out of a byte iterator (assuming a
/// UTF-8-like encoding).
///
/// # Safety
///
/// `bytes` must produce a valid UTF-8-like (UTF-8 or WTF-8) string
#[inline]
pub unsafe fn next_code_point<'a, I: Iterator<Item = &'a u8>>(bytes: &mut I) -> Option<u32> {
    let b1 = *bytes.next()?;
    if b1 < 0x80 {
        // 1 byte case (U+0000 ..= U+007F):
        // c = b1
        return Some(u32::from(b1));
    }

    // SAFETY: `bytes` produces a UTF-8-like string
    let mut next_byte = || unsafe {
        let b = *bytes.next().unwrap_unchecked();
        assert_unchecked(utf8_is_cont_byte(b));
        b
    };
    let combine = |c: u32, byte: u8| c << 6 | u32::from(byte & CONT_MASK);

    let b2 = next_byte();
    let c = u32::from(b1 & 0x1F);
    let c = combine(c, b2);
    if b1 < 0xE0 {
        // 2 byte case (U+0080 ..= U+07FF):
        // c = (b1 & 0x1F) << 6
        //   | (b2 & 0x3F) << 0
        return Some(c);
    }

    let b3 = next_byte();
    let c = combine(c, b3);
    if b1 < 0xF0 {
        // 3 byte case (U+0800 ..= U+FFFF):
        // c = (b1 & 0x1F) << 12
        //   | (b2 & 0x3F) << 6
        //   | (b3 & 0x3F) << 0
        return Some(c);
    }

    let b4 = next_byte();
    let c = combine(c, b4);
    // 4 byte case (U+01_0000 ..= U+10_FFFF):
    // c = ((b1 & 0x1F) << 18
    //    | (b2 & 0x3F) << 12
    //    | (b3 & 0x3F) << 6
    //    | (b4 & 0x3F) << 0) & 0x1F_FFFF
    Some(c & 0x1F_FFFF)
}

/// Reads the last code point out of a byte iterator (assuming a
/// UTF-8-like encoding).
///
/// # Safety
///
/// `bytes` must produce a valid UTF-8-like (UTF-8 or WTF-8) string
#[inline]
pub unsafe fn next_code_point_reverse<'a, I>(bytes: &mut I) -> Option<u32>
where I: DoubleEndedIterator<Item = &'a u8> {
    let b1 = *bytes.next_back()?;
    if b1 < 0x80 {
        // 1 byte case (U+0000 ..= U+007F):
        // c = b1
        return Some(u32::from(b1));
    }

    // SAFETY: `bytes` produces a UTF-8-like string
    let mut next_byte = || unsafe {
        let b = *bytes.next_back().unwrap_unchecked();
        assert_unchecked(!b.is_ascii());
        b
    };
    let combine = |c: u32, byte: u8, shift| c | u32::from(byte & CONT_MASK) << shift;

    let b2 = next_byte();
    let c = u32::from(b1 & CONT_MASK);
    let c = combine(c, b2, 6);
    if !utf8_is_cont_byte(b2) {
        // 2 byte case (U+0080 ..= U+07FF):
        // c = (b2 & 0x3F) << 6
        //   | (b1 & 0x3F) << 0
        return Some(c);
    }

    let b3 = next_byte();
    let c = combine(c, b3, 12);
    if !utf8_is_cont_byte(b3) {
        // 3 byte case (U+0800 ..= U+FFFF):
        // c = ((b3 & 0x3F) << 12
        //    | (b2 & 0x3F) << 6
        //    | (b1 & 0x3F) << 0) & 0xFFFF
        return Some(c & 0xFFFF);
    }

    let b4 = next_byte();
    let c = combine(c, b4, 18);
    // 4 byte case (U+01_0000 ..= U+10_FFFF):
    // c = ((b4 & 0x3F) << 18
    //    | (b3 & 0x3F) << 12
    //    | (b2 & 0x3F) << 6
    //    | (b1 & 0x3F) << 0) & 0x1F_FFFF
    Some(c & 0x1F_FFFF)
}

/// Mask of the value bits of a continuation byte (ie the lowest 6 bits).
const CONT_MASK: u8 = 0b0011_1111;

/// An iterator over the [`char`]s of a string slice.
///
///
/// This struct is created by the [`chars`] method on [`str`].
/// See its documentation for more.
///
/// [`char`]: prim@char
/// [`chars`]: str::chars
#[derive(Clone)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Chars<'a> {
    pub iter: std::slice::Iter<'a, u8>,
}

impl<'a> Chars<'a> {
    #[inline]
    pub fn new(s: &'a str) -> Self {
        Self {
            iter: s.as_bytes().iter(),
        }
    }
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<char> {
        // SAFETY: `str` invariant says `self.iter` is a valid UTF-8 string and
        // the resulting `ch` is a valid Unicode Scalar Value.
        unsafe { next_code_point(&mut self.iter).map(|ch| char::from_u32_unchecked(ch)) }
    }

    #[inline]
    fn last(mut self) -> Option<char> {
        // No need to go through the entire string.
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for Chars<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<char> {
        // SAFETY: `str` invariant says `self.iter` is a valid UTF-8 string and
        // the resulting `ch` is a valid Unicode Scalar Value.
        unsafe { next_code_point_reverse(&mut self.iter).map(|ch| char::from_u32_unchecked(ch)) }
    }
}
