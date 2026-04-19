#![allow(warnings)]

use crate::utils::simdx::movemask;
use std::{hint::select_unpredictable as select, simd::prelude::*};

pub const EOF_BYTE: u8 = 0xFF;

const LINE_COMMENT: u8 = 0xFE;
const BLOCK_COMMENT: u8 = 0xFD;
const STRING: u8 = 0xFC;
const CHAR: u8 = 0xFB;

fn eq<const N: usize>(chunk: Simd<u8, N>, byte: u8) -> Mask<i8, N> {
    chunk.simd_eq(Simd::splat(byte))
}

#[inline]
fn write_and_advance<T>(out: *mut u8, val: T) -> *mut u8 {
    unsafe {
        out.cast::<T>().write_unaligned(val);
        out.add(std::mem::size_of::<T>())
    }
}

pub unsafe fn line_comment_starts<const VEC_LEN: usize>(
    input: &[u8],
    mut out: *mut u32,
) -> *mut u32 {
    debug_assert!(input.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
    let std::ops::Range {
        start: src_start,
        end,
    } = input.as_ptr_range();

    unsafe {
        let src_end = end.sub(VEC_LEN * 2);
        let mut cursor = src_start;

        loop {
            let chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let comment_start = eq(chunk, b'/');
            let mut bitstring = movemask(comment_start);

            if bitstring != 0 {
                bitstring = bitstring.reverse_bits();
                bitstring &= bitstring << 1;
                let mut cursor = cursor;
                while bitstring != 0 {
                    let pos = bitstring.leading_zeros();
                    bitstring <<= pos;
                    bitstring <<= 2;
                    let comment_start = cursor.add(pos as usize);
                    debug_assert_eq!(comment_start.add(0).read(), b'/');
                    debug_assert_eq!(comment_start.add(1).read(), b'/');
                    cursor = comment_start.add(2);
                    let comment_offset = comment_start.offset_from_unsigned(src_start) as u32;
                    out.write(comment_offset);
                    out = out.add(1);
                }
            }
            cursor = cursor.add(VEC_LEN);
            if cursor >= src_end {
                break;
            }
        }
        out
    }
}

/// Pass 1: Remove all line comments
/// # Safety
/// The caller must ensure that `input` ends with at least `VEC_LEN * 2` bytes
/// of `EOF_BYTE`.
pub unsafe fn line_comments<const VEC_LEN: usize>(input: &[u8], mut out: *mut u8) -> *mut u8 {
    debug_assert!(input.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
    let std::ops::Range { start, end } = input.as_ptr_range();

    unsafe {
        let src_end = end.sub(VEC_LEN * 2);
        let mut cursor = start;

        'outer: loop {
            let chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            out.cast::<Simd<u8, VEC_LEN>>().write_unaligned(chunk);
            let slashes = movemask(eq(chunk, b'/')).reverse_bits();
            let slashes2 = slashes << 1 | u64::from(cursor.add(VEC_LEN + 1).read() == b'/');
            let slashes2 = slashes & slashes2;

            if slashes2 == 0 {
                out = out.add(VEC_LEN);
                cursor = cursor.add(VEC_LEN);
                if cursor >= src_end {
                    break;
                }
                continue;
            }

            let comment_start_pos = slashes2.leading_zeros() as usize;
            out = out.add(comment_start_pos);
            out = write_and_advance(out, LINE_COMMENT);

            let comment_start_ptr = cursor.add(comment_start_pos);
            debug_assert_eq!(comment_start_ptr.add(0).read(), b'/');
            debug_assert_eq!(comment_start_ptr.add(1).read(), b'/');

            let newlines = eq(chunk, b'\n');
            let newlines = movemask(newlines).reverse_bits();
            let mut newlines = (newlines << comment_start_pos) >> comment_start_pos;
            'inner: loop {
                if newlines != 0 {
                    let newline_pos = newlines.leading_zeros() as usize;
                    let comment_end_ptr = cursor.add(newline_pos);
                    debug_assert_eq!(comment_end_ptr.read(), b'\n');
                    let comment_len = comment_end_ptr.offset_from_unsigned(comment_start_ptr);
                    out = write_and_advance(out, comment_len as u32);
                    cursor = comment_end_ptr;
                    break 'inner;
                }
                cursor = cursor.add(VEC_LEN);
                let chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
                newlines = movemask(eq(chunk, b'\n')).reverse_bits();
                if cursor >= src_end {
                    let comment_end_ptr = src_end;
                    let comment_len = comment_end_ptr.offset_from_unsigned(comment_start_ptr);
                    out = write_and_advance(out, comment_len as u32);
                    break 'outer;
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use std::bstr::ByteStr;

    use super::*;
    use expect_test::{Expect, expect};

    fn check(src: &str, expect: &Expect) {
        let mut input = src.to_string().into_bytes();
        input.extend_from_slice(&[EOF_BYTE; 32]);
        let mut output = vec![0u8; input.len()];

        let out_start = output.as_ptr();
        let out_end = unsafe { line_comments::<16>(&input, output.as_mut_ptr()) };
        let out = out_start..out_end.cast_const();
        let output = unsafe { std::slice::from_ptr_range(out) };
        let output = ByteStr::new(output);
        let output = format!("{output:?}");
        let output = output.strip_prefix("\"").unwrap();
        let output = output.strip_suffix("\"").unwrap();
        expect.assert_eq(output);
    }

    #[test]
    fn empty() {
        check("", &expect![[
            r#"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
        ]]);
    }

    #[test]
    fn no_comments() {
        check("foo", &expect![[
            r"foo\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
        check("foobar", &expect![[
            r"foobar\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
        check("foobar foobar foobar foobar foobar foobar foo", &expect![[
            r"foobar foobar foobar foobar foobar foobar foo\xff\xff\xff"
        ]]);
    }

    #[test]
    fn line_comment1() {
        check("foo//\nbaz", &expect![[
            r"foo\xfe\x02\0\0\0\nbaz\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
    }

    #[test]
    fn line_comment2() {
        check("foo//bar\n", &expect![[
            r#"foo\xfe\x05\0\0\0\n\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
        ]]);
    }

    #[test]
    fn line_comment3() { check("foo//bar", &expect![[r"foo\xfe\x05\0\0\0"]]); }

    #[test]
    fn line_comment4() {
        check("//foobar\n", &expect![[
            r"\xfe\x08\0\0\0\n\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
    }

    #[test]
    fn line_comment5() { check("//foobar", &expect![[r"\xfe\x08\0\0\0"]]); }

    #[test]
    fn line_comment6() {
        check(
            "hello world\nfoo // line comment \n foooooobaaazbaaar \n // comment to EOF",
            &expect![[r"hello world\nfoo \xfe\x10\0\0\0\n foooooobaaazbaaar \n \xfe\x11\0\0\0"]],
        );
    }

    #[track_caller]
    fn check_line_comment_positions(src: &str, expect: &Expect) {
        let mut input = src.to_string().into_bytes();
        input.extend_from_slice(&[EOF_BYTE; 32]);
        let mut output = vec![0u32; input.len()];

        unsafe {
            let out = line_comment_starts::<16>(&input, output.as_mut_ptr().cast());
            let output = std::slice::from_mut_ptr_range(output.as_mut_ptr()..out);
            let output = format!("{output:?}");
            expect.assert_eq(&output);
        }
    }

    #[test]
    fn line_comment_positions() {
        check_line_comment_positions("", &expect!["[]"]);
        check_line_comment_positions("/", &expect!["[]"]);
        check_line_comment_positions("//", &expect![["[0]"]]);
        check_line_comment_positions("///", &expect![["[0]"]]);
        check_line_comment_positions("////", &expect![["[0, 2]"]]);
        check_line_comment_positions("// //", &expect![["[0, 3]"]]);
        check_line_comment_positions("   // //  ", &expect![["[3, 6]"]]);
    }
}
