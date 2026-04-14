use std::simd::prelude::*;

use crate::utils::first_set;

pub const EOF_BYTE: u8 = 0xFF;

const LINE_COMMENT: u8 = 0xFE;
const BLOCK_COMMENT: u8 = 0xFD;
const STRING: u8 = 0xFC;
const CHAR: u8 = 0xFB;

fn eq<const N: usize>(chunk: Simd<u8, N>, byte: u8) -> Mask<i8, N> {
    chunk.simd_eq(Simd::splat(byte))
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

        loop {
            let chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
            let chunk1 = cursor.add(1).cast::<Simd<u8, VEC_LEN>>().read_unaligned();

            let comment_start = eq(chunk, b'/') & eq(chunk1, b'/');
            out.cast::<Simd<u8, VEC_LEN>>().write_unaligned(chunk);
            match first_set(comment_start) {
                None => {
                    cursor = cursor.add(VEC_LEN);
                    out = out.add(VEC_LEN);
                }
                Some(pos) => {
                    let comment_start = cursor.add(pos);
                    cursor = cursor.add(pos).add(2);
                    debug_assert_eq!(cursor.sub(1).read(), b'/');
                    debug_assert_eq!(cursor.sub(2).read(), b'/');
                    out = out.add(pos);
                    out.write(LINE_COMMENT);
                    out = out.add(1);

                    loop {
                        let chunk = cursor.cast::<Simd<u8, VEC_LEN>>().read_unaligned();
                        let newline_mask = eq(chunk, b'\n');
                        match first_set(newline_mask) {
                            None => cursor = cursor.add(VEC_LEN),
                            Some(pos) => {
                                cursor = cursor.add(pos);
                                break;
                            }
                        }

                        if cursor >= src_end {
                            cursor = src_end;
                            break;
                        }
                    }

                    out.cast::<u32>()
                        .write_unaligned(cursor.offset_from_unsigned(comment_start) as u32);

                    out = out.add(4);
                }
            }

            if cursor >= src_end {
                break;
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
            r"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
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
    fn line_comment() {
        check("foo//bar\nbaz", &expect![[
            r"foo\xfe\x05\0\0\0\nbaz\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
        check("foo//bar", &expect![[r"foo\xfe\x05\0\0\0"]]);
        check("//foobar\n", &expect![[
            r"\xfe\x08\0\0\0\n\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
        check("//foobar", &expect![[r"\xfe\x08\0\0\0"]]);
        check(
            "hello world\nfoo // line comment \n foooooobaaazbaaar \n // comment to EOF",
            &expect![[r"hello world\nfoo \xfe\x10\0\0\0\n foooooobaaazbaaar \n \xfe\x11\0\0\0"]],
        );
    }
}
