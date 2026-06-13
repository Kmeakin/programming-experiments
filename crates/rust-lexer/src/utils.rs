use crate::common::{EOF_BYTE, TokenKind};

/// # Safety
/// Same as `memchr::arch::all::memchr::One::find_raw`
#[inline]
pub unsafe fn memchr_raw(needle: u8, start: *const u8, end: *const u8) -> Option<*const u8> {
    unsafe { memchr::arch::all::memchr::One::new(needle).find_raw(start, end) }
}

#[rustfmt::skip]
const LUT: [u8; 256] = {
    let mut lut = [0; 256];
    let mut i = 0;
    while i < 256 {
        // whitespace
        lut[i] |= (matches!(i as u8, | b' ' | 0x09..=0x0C) as u8);

        // digits
        lut[i] |= (matches!(i as u8, | b'_' | b'0'..=b'9') as u8) << 1;

        // ident starts
        lut[i] |= (matches!(i as u8, | b'_' | b'a'..=b'z' | b'A'..=b'Z') as u8) << 2;

        // ident conts
        lut[i] |= (matches!(i as u8, | b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') as u8) << 3;

        // punctuation
        lut[i] |= (matches!(i as u8, | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';'
                                     | b':' | b'+' | b'-' | b'*' | b'%' | b'=' | b'&' | b'|'
                                     | b'$' | b'?' | b'~' | b'#' | b'@' | b'.' | b'!' | b'>'
                                     | b'<' | b'^') as u8) << 4;
        i += 1;
    }
    lut
};

#[inline]
pub const fn is_whitespace(byte: u8) -> bool { LUT[byte as usize] & (1 << 0) != 0 }

#[inline]
pub const fn is_digit(byte: u8) -> bool { LUT[byte as usize] & (1 << 1) != 0 }

#[inline]
pub const fn is_ident_start(byte: u8) -> bool { LUT[byte as usize] & (1 << 2) != 0 }

#[inline]
pub const fn is_ident_cont(byte: u8) -> bool { LUT[byte as usize] & (1 << 3) != 0 }

#[inline]
pub const fn is_punct(b: u8) -> Option<TokenKind> {
    if LUT[b as usize] & (1 << 4) != 0 {
        unsafe { Some(std::mem::transmute::<u8, TokenKind>(b)) }
    } else {
        None
    }
}

#[inline]
pub const unsafe fn eat_whitespace(mut cursor: *const u8) -> *const u8 {
    loop {
        let array = cursor.cast::<[u8; 8]>().read();
        if !is_whitespace(array[0]) {
            return cursor.add(0);
        }
        if !is_whitespace(array[1]) {
            return cursor.add(1);
        }
        if !is_whitespace(array[2]) {
            return cursor.add(2);
        }
        if !is_whitespace(array[3]) {
            return cursor.add(3);
        }
        if !is_whitespace(array[4]) {
            return cursor.add(4);
        }
        if !is_whitespace(array[5]) {
            return cursor.add(5);
        }
        if !is_whitespace(array[6]) {
            return cursor.add(6);
        }
        if !is_whitespace(array[7]) {
            return cursor.add(7);
        }
        cursor = cursor.add(8);
    }
}

#[inline]
pub const unsafe fn eat_digits(mut cursor: *const u8) -> *const u8 {
    while is_digit(cursor.read()) {
        cursor = cursor.add(1);
    }
    cursor
}

#[inline]
pub const unsafe fn eat_ident_cont(mut cursor: *const u8) -> *const u8 {
    loop {
        let array = cursor.cast::<[u8; 8]>().read();
        if !is_ident_cont(array[0]) {
            return cursor.add(0);
        }
        if !is_ident_cont(array[1]) {
            return cursor.add(1);
        }
        if !is_ident_cont(array[2]) {
            return cursor.add(2);
        }
        if !is_ident_cont(array[3]) {
            return cursor.add(3);
        }
        if !is_ident_cont(array[4]) {
            return cursor.add(4);
        }
        if !is_ident_cont(array[5]) {
            return cursor.add(5);
        }
        if !is_ident_cont(array[6]) {
            return cursor.add(6);
        }
        if !is_ident_cont(array[7]) {
            return cursor.add(7);
        }
        cursor = cursor.add(8);
    }
}

#[inline]
pub unsafe fn line_comment(start: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"//");
    memchr_raw(b'\n', start.add(2), src_end).unwrap_or(src_end)
}

#[inline]
pub unsafe fn block_comment(start: *const u8, _src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"/*");
    let mut cursor = start.add(2);
    let mut depth = 1usize;
    loop {
        match cursor.cast::<[u8; 2]>().read() {
            [b'/', b'*', ..] => {
                cursor = cursor.add(2);
                depth += 1;
            }
            [b'*', b'/', ..] => {
                cursor = cursor.add(2);
                depth -= 1;
                if depth == 0 {
                    return cursor;
                }
            }
            [EOF_BYTE, ..] => return cursor,
            _ => cursor = cursor.add(1),
        }
    }
}

#[inline]
#[cfg(false)]
pub unsafe fn block_comment(start: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"/*");

    let mut depth = 1usize;
    let haystack = std::slice::from_ptr_range(start.add(2)..src_end);
    for pos in memchr::memchr2_iter(b'/', b'*', haystack) {
        let mut cursor = haystack.as_ptr().add(pos);
        debug_assert_matches!(cursor.read(), b'/' | b'*');
        match cursor.cast::<[u8; 2]>().read() {
            [b'/', b'*', ..] => {
                cursor = cursor.add(2);
                depth += 1;
            }
            [b'*', b'/', ..] => {
                cursor = cursor.add(2);
                depth -= 1;
                if depth == 0 {
                    return cursor;
                }
            }
            _ => cursor = cursor.add(1),
        }
    }
    src_end
}

#[inline]
pub unsafe fn single_quote_string(start: *const u8) -> *const u8 {
    debug_assert_eq!(start.read(), b'\'');
    let mut end = start.add(1);
    loop {
        match end.read() {
            b'\\' => end = end.add(2),
            b'\'' => return end.add(1),
            EOF_BYTE => return end,
            _ => end = end.add(1),
        }
    }
}

#[inline]
pub unsafe fn double_quote_string(mut cursor: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(cursor.read(), b'\"');
    cursor = cursor.add(1);

    let haystack = std::slice::from_ptr_range(cursor..src_end);
    for pos in memchr::memchr_iter(b'"', haystack) {
        let quote = haystack.as_ptr().add(pos);
        debug_assert_eq!(quote.read(), b'\"');

        let after_quote = quote.add(1);

        let mut num_backslashes = 0usize;
        let mut backslash_ptr = quote.sub(1);
        while backslash_ptr.read() == b'\\' {
            backslash_ptr = backslash_ptr.sub(1);
            num_backslashes += 1;
        }
        if num_backslashes.is_multiple_of(2) {
            return after_quote;
        }
    }
    src_end
}

#[inline]
pub unsafe fn raw_string(start: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(start.cast::<[u8; 2]>().read(), *b"r\"");
    match memchr_raw(b'"', start.add(2), src_end) {
        Some(end) => end.add(1),
        None => src_end,
    }
}

#[inline]
pub unsafe fn raw_hash_string(cursor: *const u8, src_end: *const u8) -> *const u8 {
    debug_assert_eq!(cursor.cast::<[u8; 2]>().read(), *b"r#");
    let mut cursor = cursor.add(2);
    let mut num_hashes = 1usize;
    while cursor.read() == b'#' {
        cursor = cursor.add(1);
        num_hashes += 1;
    }

    if cursor.read() != b'\"' {
        return cursor;
    }
    cursor = cursor.add(1);

    let haystack = std::slice::from_ptr_range(cursor..src_end);
    for pos in memchr::memchr_iter(b'"', haystack) {
        cursor = haystack.as_ptr().add(pos);
        cursor = cursor.add(1);
        let mut num_hashes = num_hashes;
        while cursor.read() == b'#' {
            cursor = cursor.add(1);
            num_hashes -= 1;
            if num_hashes == 0 {
                return cursor;
            }
        }
    }
    src_end
}
