#![cfg(false)]

use std::{bstr::ByteStr, range, simd::prelude::*};

use crate::TokenKind;

pub const EOF_BYTE: u8 = 0xFF;
pub const VEC_LEN: usize = 16;
pub const EOF_PADDING: usize = VEC_LEN * 2;

/*
  0 1 2 3 4 5 6 7 8 9 a b c d e f
0                  \t\n    \r
1
2   ! " # $ % & ' ( ) * + , - . /
3 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
4 @ A B C D E F G H I J K L M N O
5 P Q R S T U V W X Y Z [ \ ] ^ _
6 ` a b c d e f g h i j k l m n o
7 p q r s t u v w x y z { | } ~ del
*/

type Chunk = Simd<u8, 16>;

fn range(input: Chunk, range: std::ops::RangeInclusive<u8>) -> Mask<i8, 16> {
    Simd::splat(*range.start()).simd_le(input) & (input.simd_le(Simd::splat(*range.end())))
}

fn eq(input: Chunk, byte: u8) -> Mask<i8, 16> { input.simd_eq(Simd::splat(byte)) }

fn prefix_xor(mut mask: u64) -> u64 {
    mask ^= mask << 1;
    mask ^= mask << 2;
    mask ^= mask << 4;
    mask ^= mask << 8;
    mask ^= mask << 16;
    mask ^= mask << 32;
    mask
}

/// Return a mask which is true for bytes inside a line comment, false for all
/// other bytes.
/// eg `// comment\n` -> `1111111111111110`
fn line_comment(input: Chunk) -> u64 {
    eprintln!("input:\t\t{}", ByteStr::new(input.as_array()));

    let slash = eq(input, b'/').to_bitmask().reverse_bits();
    eprintln!("slash:\t\t{slash:064b}");

    let slash2 = slash & slash << 1;
    eprintln!("slash2:\t\t{slash2:064b}");

    let newline = eq(input, b'\n').to_bitmask().reverse_bits();
    eprintln!("newline:\t{newline:064b}");

    let slash2_suffix = prefix_xor(slash2.reverse_bits()).reverse_bits();
    eprintln!("slash2_suffix:\t{slash2_suffix:064b}");

    let newline_suffix = prefix_xor(newline.reverse_bits()).reverse_bits();
    eprintln!("newline_suffix:\t{newline_suffix:064b}");

    let newline_prefix = !newline_suffix;
    eprintln!("newline_prefix:\t{newline_prefix:064b}");

    let result = slash2_suffix & newline_prefix;
    eprintln!("result:\t\t{result:064b}");

    result
}

#[cfg(test)]
mod line_comment_tests {
    use expect_test::Expect;
    use expect_test::expect;

    use super::*;

    fn check(src: [u8; 16], expect: &Expect) {
        let chunk = Simd::from_array(src);
        let mask = line_comment(chunk);
        let output = mask;
        expect.assert_eq(&format!("{output:064b}"));
    }

    #[test]
    fn simple() {
        check(*b"1234// comment\no", &expect![[
            r"0000111111111100000000000000000000000000000000000000000000000000"
        ]]);
    }

    #[test]
    fn no_newline() {
        check(*b"// commentfooooo", &expect![[
            r"1111111111111111111111111111111111111111111111111111111111111111"
        ]]);
    }

    #[test]
    fn multiple_comments() { check(*b"// comment\n// c\n", &expect![[r"1111111111111101"]]); }
}

pub fn lex(src: &[u8], mut output: *mut u8) -> (*const u8, *mut u8) {
    debug_assert!(src.ends_with(&[EOF_BYTE; EOF_PADDING]));
    let std::ops::Range {
        start: mut src_start,
        end: src_end,
    } = src.as_ptr_range();
    let src_end = unsafe { src_end.sub(EOF_PADDING) };
    let chunk: Chunk = unsafe { src_start.cast::<Chunk>().read_unaligned() };

    let eof_mask = eq(chunk, EOF_BYTE);
    let digit_mask = range(chunk, b'0'..=b'9');

    let newlines = eq(chunk, b'\n');
    let whitespace_mask = range(chunk, b'\t'..=b'\r') | eq(chunk, b' ');
    let control_mask = (range(chunk, 0x00..=0x1F) & !whitespace_mask) | eq(chunk, 0x7F);
    let nonascii_mask = chunk.simd_ge(Simd::splat(0x80));

    let alpha_mask = range(chunk, b'a'..=b'z') | range(chunk, b'A'..=b'Z');
    let underscore_mask = eq(chunk, b'_');
    let ident_mask = alpha_mask | underscore_mask | digit_mask;

    let slash_mask = eq(chunk, b'/');
    let star_mask = eq(chunk, b'*');

    let slash_slash_mask = slash_mask & slash_mask.shift_elements_left::<1>(false);
    let slash_star_mask = slash_mask & star_mask.shift_elements_left::<1>(false);

    let single_quote_mask = eq(chunk, b'\'');
    let double_quote_mask = eq(chunk, b'"');

    let punct_mask = range(chunk, b'!'..=b'~')
        & !ident_mask
        & !double_quote_mask
        & !single_quote_mask
        & !slash_slash_mask
        & !slash_star_mask;

    unsafe {
        output.cast::<Chunk>().write_unaligned(chunk);

        src_start = src_start.add(VEC_LEN);
        output = output.add(VEC_LEN);
        (src_start, output)
    }
}

#[cfg(test)]
mod tests {
    use std::bstr::ByteStr;

    use super::*;
    use expect_test::{Expect, expect};

    // #[track_caller]
    fn check(src: &str, expect: &Expect) {
        let mut input = src.to_string().into_bytes();
        input.extend(&[EOF_BYTE; EOF_PADDING]);
        let mut output = vec![EOF_BYTE; input.len()];
        let (_src_ptr, output_ptr) = lex(&input, output.as_mut_ptr());
        let output =
            unsafe { std::slice::from_ptr_range(output.as_ptr()..output_ptr.cast_const()) };
        let output = format!("{:?}", ByteStr::new(output));
        // strip outermost quotes
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
    fn punct() {
        check("!", &expect![[
            r"!\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
        ]]);
    }
}
