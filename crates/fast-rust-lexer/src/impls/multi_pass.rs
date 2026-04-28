#![allow(warnings)]

use crate::utils::{bitstring::BitString, simdx::movemask};
use std::{bstr::ByteStr, hint::select_unpredictable as select, ptr, simd::prelude::*};

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

struct NewlineMask {
    bits: u64,
}

impl NewlineMask {
    fn from_ptr<const VEC_LEN: usize>(ptr: *const u8) -> Self {
        let vec = unsafe { ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned() };
        Self::from_vec(vec)
    }

    fn from_vec<const VEC_LEN: usize>(vec: Simd<u8, VEC_LEN>) -> Self {
        let bits = movemask(eq(vec, b'\n')).reverse_bits();
        Self { bits }
    }

    fn first_set(&self) -> Option<usize> {
        if self.bits == 0 {
            None
        } else {
            Some(self.bits.leading_zeros() as usize)
        }
    }

    fn clear(&mut self, upto: usize) { self.bits &= u64::MAX >> upto; }
}

#[derive(Debug)]
struct LineCommentMask {
    bits: u64,
}

impl LineCommentMask {
    unsafe fn from_ptr<const VEC_LEN: usize>(ptr: *const u8) -> Self {
        let vec = ptr.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
        let bits = movemask(eq(vec, b'/')).reverse_bits();
        let bits1 = bits << 1 | u64::from(ptr.add(VEC_LEN + 1).read() == b'/');
        Self { bits }
    }

    fn first_set(&self) -> Option<usize> {
        if self.bits == 0 {
            None
        } else {
            Some(self.bits.leading_zeros() as usize)
        }
    }

    fn clear(&mut self, upto: usize) { self.bits &= u64::MAX >> upto; }
}

struct Cursor<const VEC_LEN: usize> {
    cur: *const u8,
    src_end: *const u8,
    newlines: BitString,
    line_comments: BitString,
}

impl<const VEC_LEN: usize> Cursor<VEC_LEN> {
    fn new(input: &[u8]) -> Self {
        unsafe {
            let std::ops::Range { start, end } = input.as_ptr_range();

            let chunk = start.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let newlines = BitString::new(movemask(eq(chunk, b'\n')).reverse_bits());
            let slashes = movemask(eq(chunk, b'/')).reverse_bits();
            let slashes1 = slashes << 1 | u64::from(start.add(VEC_LEN + 1).read() == b'/');
            let line_comments = BitString::new(slashes & slashes1);

            Self {
                cur: start,
                src_end: end,
                newlines,
                line_comments,
            }
        }
    }

    fn refill(&mut self) -> bool {
        unsafe {
            if self.cur.add(VEC_LEN) >= self.src_end {
                return false;
            }
            self.cur = self.cur.add(VEC_LEN);

            let chunk = unsafe { self.cur.cast::<Simd<u8, VEC_LEN>>().read_unaligned() };
            let newlines = BitString::new(movemask(eq(chunk, b'\n')).reverse_bits());
            let slashes = movemask(eq(chunk, b'/')).reverse_bits();
            let slashes1 = slashes << 1 | u64::from(self.cur.add(VEC_LEN + 1).read() == b'/');
            let line_comments = BitString::new(slashes & slashes1);
            self.newlines = newlines;
            self.line_comments = line_comments;
            true
        }
    }

    fn next_line_comment(&mut self) -> Option<*const u8> {
        unsafe {
            let pos = match self.line_comments.first_set() {
                Some(pos) => pos,
                None => loop {
                    match self.refill() {
                        true => match self.line_comments.first_set() {
                            Some(pos) => break pos,
                            None => continue,
                        },
                        false => return None,
                    }
                },
            };

            let comment_start_ptr = self.cur.add(pos);
            debug_assert_eq!(comment_start_ptr.add(0).read(), b'/');
            debug_assert_eq!(comment_start_ptr.add(1).read(), b'/');

            self.line_comments = self.line_comments.clear_upto(pos + 1);
            self.newlines = self.newlines.clear_upto(pos + 1);
            Some(comment_start_ptr)
        }
    }

    fn next_newline(&mut self) -> Option<*const u8> {
        unsafe {
            let pos = match self.newlines.first_set() {
                Some(pos) => pos,
                None => loop {
                    match self.refill() {
                        true => match self.newlines.first_set() {
                            Some(pos) => break pos,
                            None => continue,
                        },
                        false => return None,
                    }
                },
            };
            let newline_ptr = self.cur.add(pos);
            debug_assert_eq!(newline_ptr.add(0).read(), b'\n');

            self.line_comments = self.line_comments.clear_upto(pos + 1);
            self.newlines = self.newlines.clear_upto(pos + 1);
            Some(newline_ptr)
        }
    }
}

pub unsafe fn line_comments<const VEC_LEN: usize>(input: &[u8], mut out: *mut u8) -> *mut u8 {
    debug_assert!(input.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
    unsafe {
        let input = input
            .strip_suffix([[EOF_BYTE; VEC_LEN]; 2].as_flattened())
            .unwrap_unchecked();
        let mut cursor = Cursor::<VEC_LEN>::new(input);
        loop {
            match cursor.next_line_comment() {
                None => break,
                Some(comment_start_ptr) => {
                    out = write_and_advance(out, LINE_COMMENT);
                    let Some(newline_ptr) = cursor.next_newline() else {
                        let eof_ptr = cursor.src_end;
                        let len = eof_ptr.offset_from_unsigned(comment_start_ptr) as u32;
                        out = write_and_advance(out, len);
                        break;
                    };
                    let len = newline_ptr.offset_from_unsigned(comment_start_ptr) as u32;
                    out = write_and_advance(out, len);
                }
            }
        }
        out
    }
}

/// Pass 1: Remove all line comments
/// # Safety
/// The caller must ensure that `input` ends with at least `VEC_LEN * 2` bytes
/// of `EOF_BYTE`.
#[cfg(false)]
pub unsafe fn line_comments<const VEC_LEN: usize>(input: &[u8], mut out: *mut u8) -> *mut u8 {
    debug_assert!(input.ends_with([[EOF_BYTE; VEC_LEN]; 2].as_flattened()));
    let std::ops::Range { start, end } = input.as_ptr_range();

    unsafe {
        let src_end = end.sub(VEC_LEN * 2);
        let mut cursor = start;

        let mut chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
        out.cast::<Simd<u8, VEC_LEN>>().write_unaligned(chunk);
        let mut newlines = movemask(eq(chunk, b'\n')).reverse_bits();
        let mut slashes = {
            let first_slashes = movemask(eq(chunk, b'/')).reverse_bits();
            let second_slashes = first_slashes << 1;
            first_slashes & second_slashes
        };

        let mut iter = 0;
        'outer: loop {
            eprintln!("iter\t= {iter}");
            eprintln!("chunk\t= {:?}", ByteStr::new(&chunk.to_array()));
            eprintln!("slashes\t=  {slashes:064b}");
            eprintln!("newline\t=  {newlines:064b}");
            iter += 1;

            if slashes == 0 {
                eprintln!("No '//' found, writing chunk and advancing");

                // out = write_and_advance(out, chunk);
                cursor = cursor.add(VEC_LEN);
                if cursor >= src_end {
                    break;
                }

                // Refill the bitmasks
                chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
                newlines = movemask(eq(chunk, b'\n')).reverse_bits();
                slashes = {
                    let first_slashes = movemask(eq(chunk, b'/')).reverse_bits();
                    let second_slashes =
                        first_slashes << 1 | u64::from(cursor.add(VEC_LEN + 1).read() == b'/');
                    first_slashes & second_slashes
                };
                continue;
            }

            let comment_start_pos = slashes.leading_zeros() as usize;

            out = out.add(comment_start_pos);
            out = write_and_advance(out, LINE_COMMENT);

            let comment_start_ptr = cursor.add(comment_start_pos);

            debug_assert_eq!(comment_start_ptr.add(0).read(), b'/');
            debug_assert_eq!(comment_start_ptr.add(1).read(), b'/');

            newlines &= (u64::MAX >> comment_start_pos);
            slashes &= (u64::MAX >> comment_start_pos);

            'inner: loop {
                if newlines != 0 {
                    let newline_pos = newlines.leading_zeros() as usize;
                    newlines &= (u64::MAX >> newline_pos);
                    slashes &= (u64::MAX >> newline_pos);
                    let comment_end_ptr = cursor.add(newline_pos);
                    debug_assert_eq!(comment_end_ptr.read(), b'\n');
                    let comment_len = comment_end_ptr.offset_from_unsigned(comment_start_ptr);
                    out = write_and_advance(out, comment_len as u32);
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
    fn empty() { check("", &expect![""]); }

    #[test]
    fn no_comments() {
        check("foo", &expect![""]);
        check("foobar", &expect![""]);
        check("foobar foobar foobar foobar foobar foobar foo", &expect![
            ""
        ]);
    }

    #[test]
    fn line_comment1() { check("foo//\nbaz", &expect![[r#"\xfe\x02\0\0\0"#]]); }

    #[test]
    fn line_comment2() { check("foo//bar\n", &expect![[r#"\xfe\x05\0\0\0"#]]); }

    #[test]
    fn line_comment3() { check("foo//bar", &expect![[r#"\xfe\x05\0\0\0"#]]); }

    #[test]
    fn line_comment4() { check("//foobar\n", &expect![[r#"\xfe\x08\0\0\0"#]]); }

    #[test]
    fn line_comment5() { check("//foobar", &expect![[r"\xfe\x08\0\0\0"]]); }

    #[test]
    fn line_comment6() {
        check(
            "hello world\nfoo // line comment \n foooooobaaazbaaar \n // comment to EOF",
            &expect![[r#"\xfe\x10\0\0\0\xfe\x11\0\0\0"#]],
        );
    }

    #[test]
    fn line_comments7() {
        let src = r###"
/// Pass 1: Remove all line comments
/// # Safety
/// The caller must ensure that `input` ends with at least `VEC_LEN * 2` bytes
/// of `EOF_BYTE`.
pub unsafe fn line_comments<const VEC_LEN: usize>(input: &[u8], mut out: *mut u8) -> *mut u8 {
"###;
        check(src, &expect![[
            r#"\xfe$\0\0\0\xfe\x0c\0\0\0\xfeN\0\0\0\xfe\x12\0\0\0"#
        ]]);
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
