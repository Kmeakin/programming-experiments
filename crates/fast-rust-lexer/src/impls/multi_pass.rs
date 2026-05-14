#![allow(clippy::wildcard_imports)]

use std::ops::{BitOr, Shr};
use std::simd::prelude::*;

use crate::TokenKind;
use crate::utils::simdx::*;
use crate::utils::{align_down, is_punct, write_punct, write_token};

pub const EOF_BYTE: u8 = 0xFF;
const NUM_VECS: usize = 3;

pub fn prepare_input<W: Word, const VEC_LEN: usize>(src: &str) -> (Vec<u8>, [Vec<W>; NUM_VECS]) {
    unsafe {
        let size = src.len() + VEC_LEN * 2;
        let layout = std::alloc::Layout::from_size_align(size, VEC_LEN).unwrap();
        let ptr = std::alloc::alloc(layout);
        assert!(!ptr.is_null());
        let mut input_vec = Vec::from_raw_parts(ptr, 0, size);
        input_vec.extend(src.as_bytes());
        input_vec.extend([EOF_BYTE; VEC_LEN]);
        input_vec.extend([EOF_BYTE; VEC_LEN]);
        assert!(input_vec.as_ptr().is_aligned_to(VEC_LEN));

        let num_words = input_vec.len().div_ceil(VEC_LEN);
        let out_vecs = [
            vec![W::ZERO; num_words],
            vec![W::ZERO; num_words],
            vec![W::ZERO; num_words],
        ];
        (input_vec, out_vecs)
    }
}

pub trait Word: Copy + Shr<usize, Output = Self> {
    const ZERO: Self;

    type Vec: Copy;
    type Mask: Copy + BitOr<Output = Self::Mask>;

    /// # Safety
    /// Usual pointer validity rules.
    unsafe fn load(ptr: *const u8) -> Self::Vec;
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask;
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask;
    fn movemask(mask: Self::Mask) -> Self;
    fn trailing_zeros(self) -> usize;
    fn trailing_ones(self) -> usize;
}

impl Word for u16 {
    const ZERO: Self = 0;

    type Vec = Simd<u8, 16>;
    type Mask = Mask<i8, 16>;

    unsafe fn load(ptr: *const u8) -> Self::Vec { unsafe { load::<16>(ptr) } }
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask { vec.simd_eq(Simd::splat(byte)) }
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask {
        Simd::splat(start).simd_le(vec) & vec.simd_le(Simd::splat(end))
    }
    fn movemask(mask: Self::Mask) -> Self { movemask(mask) as Self }
    fn trailing_zeros(self) -> usize { self.trailing_zeros() as usize }
    fn trailing_ones(self) -> usize { self.trailing_ones() as usize }
}

impl Word for u32 {
    const ZERO: Self = 0;

    type Vec = Simd<u8, 32>;
    type Mask = Mask<i8, 32>;

    unsafe fn load(ptr: *const u8) -> Self::Vec { unsafe { load::<32>(ptr) } }
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask { vec.simd_eq(Simd::splat(byte)) }
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask {
        Simd::splat(start).simd_le(vec) & vec.simd_le(Simd::splat(end))
    }
    fn movemask(mask: Self::Mask) -> Self { movemask(mask) as Self }
    fn trailing_zeros(self) -> usize { self.trailing_zeros() as usize }
    fn trailing_ones(self) -> usize { self.trailing_ones() as usize }
}

impl Word for u64 {
    const ZERO: Self = 0;

    type Vec = Simd<u8, 64>;
    type Mask = Mask<i8, 64>;

    unsafe fn load(ptr: *const u8) -> Self::Vec { unsafe { load::<64>(ptr) } }
    fn eq(vec: Self::Vec, byte: u8) -> Self::Mask { vec.simd_eq(Simd::splat(byte)) }
    fn in_range(vec: Self::Vec, start: u8, end: u8) -> Self::Mask {
        Simd::splat(start).simd_le(vec) & vec.simd_le(Simd::splat(end))
    }
    fn movemask(mask: Self::Mask) -> Self { movemask(mask) }
    fn trailing_zeros(self) -> usize { self.trailing_zeros() as usize }
    fn trailing_ones(self) -> usize { self.trailing_ones() as usize }
}

pub fn stage1_16(src: &[u8], out: &mut [Vec<u16>; NUM_VECS]) { stage1::<u16, 16>(src, out) }
pub fn stage1_32(src: &[u8], out: &mut [Vec<u32>; NUM_VECS]) { stage1::<u32, 32>(src, out) }
pub fn stage1_64(src: &[u8], out: &mut [Vec<u64>; NUM_VECS]) { stage1::<u64, 64>(src, out) }

pub fn stage1<W: Word, const VEC_LEN: usize>(src: &[u8], out: &mut [Vec<W>; NUM_VECS]) {
    const { assert!(VEC_LEN == size_of::<W>() * 8) }

    debug_assert_eq!(src.last_chunk(), Some(&[EOF_BYTE; VEC_LEN]));
    for vec in out.iter() {
        debug_assert!(vec.len() >= src.len().div_ceil(VEC_LEN));
    }

    let mut ptr = src.as_ptr();
    let src_end = src.as_ptr_range().end;

    let [newlines, ident_vec, double_quote_vec] = out;
    let mut newlines_ptr = newlines.as_mut_ptr();
    let mut ident_ptr = ident_vec.as_mut_ptr();
    let mut double_quote_ptr = double_quote_vec.as_mut_ptr();

    unsafe {
        while ptr < src_end {
            let vec = W::load(ptr);

            let newlines = W::movemask(W::eq(vec, b'\n'));
            let idents = W::movemask(
                W::eq(vec, b'_')
                    | W::in_range(vec, b'0', b'9')
                    | W::in_range(vec, b'a', b'z')
                    | W::in_range(vec, b'A', b'Z'),
            );
            let double_quotes = W::movemask(W::eq(vec, b'"'));

            newlines_ptr.write(newlines);
            ident_ptr.write(idents);
            double_quote_ptr.write(double_quotes);

            newlines_ptr = newlines_ptr.add(1);
            ident_ptr = ident_ptr.add(1);
            double_quote_ptr = double_quote_ptr.add(1);
            ptr = ptr.add(VEC_LEN);
        }
    }
}

pub fn stage2_16<'out>(
    src: &[u8],
    bitmasks: [&[u16]; NUM_VECS],
    out: &'out mut [u8],
) -> &'out mut [u8] {
    stage2::<u16, 16>(src, bitmasks, out)
}

pub fn stage2_32<'out>(
    src: &[u8],
    bitmasks: [&[u32]; NUM_VECS],
    out: &'out mut [u8],
) -> &'out mut [u8] {
    stage2::<u32, 32>(src, bitmasks, out)
}

pub fn stage2_64<'out>(
    src: &[u8],
    bitmasks: [&[u64]; NUM_VECS],
    out: &'out mut [u8],
) -> &'out mut [u8] {
    stage2::<u64, 64>(src, bitmasks, out)
}

pub fn stage2<'out, W: Word, const VEC_LEN: usize>(
    src: &[u8],
    bitmasks: [&[W]; NUM_VECS],
    out: &'out mut [u8],
) -> &'out mut [u8] {
    debug_assert!(out.len() >= src.len() * 5); // Each token is at most 5 bytes (kind + len)

    unsafe {
        let src_end = src.as_ptr_range().end.sub(VEC_LEN * 2);
        let out_range = out.as_mut_ptr_range();
        let out_end = stage2_inner::<W, VEC_LEN>(src.as_ptr(), src_end, bitmasks, out_range.start);
        let out_len = out_end.offset_from_unsigned(out_range.start);
        &mut out[..out_len]
    }
}

fn stage2_inner<W: Word, const VEC_LEN: usize>(
    mut src: *const u8,
    src_end: *const u8,
    bitmasks: [&[W]; NUM_VECS],
    mut out: *mut u8,
) -> *mut u8 {
    unsafe {
        let src_start = src;
        loop {
            let token_start = src;
            let byte = src.read();
            match byte {
                | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-'
                | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.'
                | b'!' | b'>' | b'<' | b'^' => {
                    let mut byte = byte;
                    loop {
                        out = write_punct(out, byte);
                        src = src.add(1);
                        byte = src.read();
                        if !is_punct(byte) {
                            break;
                        }
                    }
                }

                b' ' | b'\t' | b'\n' => {
                    src = src.add(1);
                    while let b' ' | b'\t' | b'\n' = src.read() {
                        src = src.add(1);
                    }
                    out = write_token(out, TokenKind::Whitespace, token_start, src);
                }

                b'/' => match src.add(1).read() {
                    b'/' => {
                        src = src.add(2);
                        src = eat_line_comment::<W, VEC_LEN>(src_start, src, src_end, bitmasks);
                        out = write_token(out, TokenKind::LineComment, token_start, src);
                    }
                    b'*' => {
                        src = src.add(2);
                        let mut depth = 1u32;
                        loop {
                            match &src.cast::<[u8; 2]>().read() {
                                b"*/" => {
                                    src = src.add(2);
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                b"/*" => {
                                    src = src.add(2);
                                    depth += 1;
                                }
                                [EOF_BYTE, _] => break,
                                _ => src = src.add(1),
                            }
                        }
                        out = write_token(out, TokenKind::BlockComment, token_start, src);
                    }
                    _ => {
                        src = src.add(1);
                        out = write_punct(out, b'/');
                    }
                },

                b'"' => {
                    src = eat_string::<W, VEC_LEN>(src, src_end, bitmasks);
                    out = write_token(out, TokenKind::Str, token_start, src);
                }
                b'\'' => {
                    src = src.add(1);
                    if let b'a'..=b'z' | b'A'..=b'Z' | b'_' = src.read() {
                        src = src.add(1);
                        while let b'a'..=b'z' | b'A'..=b'Z' | b'_' = src.read() {
                            src = src.add(1);
                        }

                        match src.read() {
                            b'\'' => {
                                src = src.add(1);
                                out = write_token(out, TokenKind::Char, token_start, src);
                            }
                            _ => out = write_token(out, TokenKind::Lifetime, token_start, src),
                        }
                        continue;
                    }

                    loop {
                        match src.read() {
                            b'\'' => {
                                let mut src_back = src.sub(1);
                                src = src.add(1);
                                let mut backslashes = 0;
                                while src_back.read() == b'\\' {
                                    backslashes += 1;
                                    src_back = src_back.sub(1);
                                }
                                if backslashes % 2 == 0 {
                                    break;
                                }
                            }
                            EOF_BYTE => break,
                            _ => src = src.add(1),
                        }
                    }
                    out = write_token(out, TokenKind::Char, token_start, src);
                }

                b'b' => match &src.add(1).cast::<[u8; 2]>().read() {
                    [b'r', b'"'] => {
                        src = src.add(2);
                        src = eat_raw_string::<W, VEC_LEN>(src_start, src, src_end, bitmasks);
                        out = write_token(out, TokenKind::RawByteStr, token_start, src);
                    }
                    [b'r', b'#'] => {
                        src = src.add(2);
                        src = eat_hash_string::<W, VEC_LEN>(src, src_end, bitmasks);
                        out = write_token(out, TokenKind::RawByteStr, token_start, src);
                    }
                    [b'"', _] => {
                        src = src.add(1);
                        src = eat_string::<W, VEC_LEN>(src, src_end, bitmasks);
                        out = write_token(out, TokenKind::ByteStr, token_start, src);
                    }
                    [b'\'', _] => {
                        src = src.add(2);
                        loop {
                            match src.read() {
                                b'\'' => {
                                    let mut src_back = src.sub(1);
                                    src = src.add(1);
                                    let mut backslashes = 0;
                                    while src_back.read() == b'\\' {
                                        backslashes += 1;
                                        src_back = src_back.sub(1);
                                    }
                                    if backslashes % 2 == 0 {
                                        break;
                                    }
                                }
                                EOF_BYTE => break,
                                _ => src = src.add(1),
                            }
                        }
                        out = write_token(out, TokenKind::Byte, token_start, src);
                    }
                    _ => {
                        src = eat_ident::<W, VEC_LEN>(src_start, src, bitmasks);
                        out = write_token(out, TokenKind::Ident, token_start, src);
                    }
                },
                b'c' => match &src.add(1).cast::<[u8; 2]>().read() {
                    [b'r', b'"'] => {
                        src = src.add(2);
                        src = eat_raw_string::<W, VEC_LEN>(src_start, src, src_end, bitmasks);
                        out = write_token(out, TokenKind::RawCStr, token_start, src);
                    }
                    [b'r', b'#'] => {
                        src = src.add(2);
                        src = eat_hash_string::<W, VEC_LEN>(src, src_end, bitmasks);
                        out = write_token(out, TokenKind::RawCStr, token_start, src);
                    }
                    [b'"', _] => {
                        src = src.add(1);
                        src = eat_string::<W, VEC_LEN>(src, src_end, bitmasks);
                        out = write_token(out, TokenKind::CStr, token_start, src);
                    }
                    _ => {
                        src = eat_ident::<W, VEC_LEN>(src_start, src, bitmasks);
                        out = write_token(out, TokenKind::Ident, token_start, src);
                    }
                },
                b'r' => match &src.add(1).cast::<[u8; 2]>().read() {
                    [b'"', _] => {
                        src = src.add(1);
                        src = eat_raw_string::<W, VEC_LEN>(src_start, src, src_end, bitmasks);
                        out = write_token(out, TokenKind::RawStr, token_start, src);
                    }
                    [b'#', b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'] => {
                        src = src.add(2);
                        src = eat_ident::<W, VEC_LEN>(src_start, src, bitmasks);
                        out = write_token(out, TokenKind::RawIdent, token_start, src);
                    }
                    [b'#', _] => {
                        src = src.add(1);
                        src = eat_hash_string::<W, VEC_LEN>(src, src_end, bitmasks);
                        out = write_token(out, TokenKind::RawStr, token_start, src);
                    }
                    _ => {
                        src = eat_ident::<W, VEC_LEN>(src_start, src, bitmasks);
                        out = write_token(out, TokenKind::Ident, token_start, src);
                    }
                },

                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    src = eat_ident::<W, VEC_LEN>(src_start, src, bitmasks);
                    out = write_token(out, TokenKind::Ident, token_start, src);
                }
                b'0'..=b'9' => {
                    src = src.add(1);
                    while let b'0'..=b'9' | b'_' = src.read() {
                        src = src.add(1);
                    }

                    let mut kind = match src.cast::<[u8; 2]>().read() {
                        [b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_'] => {
                            out = write_token(out, TokenKind::Int, token_start, src);
                            continue;
                        }
                        [b'.', _] => {
                            src = src.add(1);
                            while let b'0'..=b'9' | b'_' = src.read() {
                                src = src.add(1);
                            }
                            TokenKind::Float
                        }
                        _ => TokenKind::Int,
                    };

                    if let b'e' | b'E' = src.read() {
                        src = src.add(1);
                        kind = TokenKind::Float;
                        src = src.add(usize::from(matches!(src.read(), b'+' | b'-')));
                    }

                    src = eat_ident::<W, VEC_LEN>(src_start, src, bitmasks);
                    out = write_token(out, kind, token_start, src);
                }

                EOF_BYTE => return out,
                _ => {
                    src = src.add(1);
                    out = write_token(out, TokenKind::Unknown, token_start, src);
                }
            }
        }
    }
}

fn eat_whitespace<W: Word, const VEC_LEN: usize>(
    src_start: *const u8,
    mut src: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        let byte_idx = src.offset_from_unsigned(src_start);
        let mut word_idx = byte_idx / VEC_LEN;
        let mut chunk_offset = byte_idx % VEC_LEN;

        loop {
            let word = bitmasks[0][word_idx];
            let len = (word >> chunk_offset).trailing_ones();
            src = src.add(len);
            if chunk_offset + len < VEC_LEN {
                break;
            }
            chunk_offset = 0;
            word_idx += 1;
        }

        src
    }
}

fn eat_line_comment<W: Word, const VEC_LEN: usize>(
    src_start: *const u8,
    mut src: *const u8,
    src_end: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        let byte_idx = src.offset_from_unsigned(src_start);
        let mut word_idx = byte_idx / VEC_LEN;
        let mut chunk_ptr = align_down::<VEC_LEN>(src);
        let mut chunk_offset = src.offset_from_unsigned(chunk_ptr);

        loop {
            let word = bitmasks[0][word_idx];
            let len = (word >> chunk_offset).trailing_zeros();
            if chunk_offset + len < VEC_LEN {
                src = chunk_ptr.add(chunk_offset + len);
                debug_assert_eq!(src.read(), b'\n');
                return src;
            }
            chunk_offset = 0;
            word_idx += 1;
            chunk_ptr = chunk_ptr.add(VEC_LEN);
            if chunk_ptr >= src_end {
                return src_end;
            }
        }
    }
}

fn eat_ident<W: Word, const VEC_LEN: usize>(
    src_start: *const u8,
    mut src: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        let byte_idx = src.offset_from_unsigned(src_start);
        let mut word_idx = byte_idx / VEC_LEN;
        let mut chunk_offset = byte_idx % VEC_LEN;

        loop {
            let word = bitmasks[1][word_idx];
            let len = (word >> chunk_offset).trailing_ones();
            src = src.add(len);
            if chunk_offset + len < VEC_LEN {
                break;
            }
            chunk_offset = 0;
            word_idx += 1;
        }

        src
    }
}

fn eat_raw_string<W: Word, const VEC_LEN: usize>(
    src_start: *const u8,
    mut src: *const u8,
    src_end: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        debug_assert_eq!(src.read(), b'"');
        src = src.add(1);

        let byte_idx = src.offset_from_unsigned(src_start);
        let mut word_idx = byte_idx / VEC_LEN;
        let mut chunk_ptr = align_down::<VEC_LEN>(src);
        let mut chunk_offset = src.offset_from_unsigned(chunk_ptr);

        loop {
            let word = bitmasks[2][word_idx];
            let len = (word >> chunk_offset).trailing_zeros();
            if chunk_offset + len < VEC_LEN {
                src = chunk_ptr.add(chunk_offset + len);
                debug_assert_eq!(src.read(), b'"');
                src = src.add(1);
                return src;
            }
            chunk_offset = 0;
            word_idx += 1;
            chunk_ptr = chunk_ptr.add(VEC_LEN);
            if chunk_ptr >= src_end {
                return src_end;
            }
        }
    }
}

#[cfg(false)]
fn eat_raw_string<W: Word, const VEC_LEN: usize>(
    mut src: *const u8,
    src_end: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        debug_assert_eq!(src.read(), b'"');
        src = src.add(1);

        loop {
            match src.read() {
                b'"' => {
                    src = src.add(1);
                    break;
                }
                EOF_BYTE => break,
                _ => src = src.add(1),
            }
        }
        src
    }
}

fn eat_string<W: Word, const VEC_LEN: usize>(
    mut src: *const u8,
    src_end: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        debug_assert_eq!(src.read(), b'"');
        src = src.add(1);

        loop {
            match src.read() {
                b'"' => {
                    let mut src_back = src.sub(1);
                    src = src.add(1);
                    let mut backslashes = 0;
                    while src_back.read() == b'\\' {
                        backslashes += 1;
                        src_back = src_back.sub(1);
                    }
                    if backslashes % 2 == 0 {
                        break;
                    }
                }
                EOF_BYTE => break,
                _ => src = src.add(1),
            }
        }
        src
    }
}

fn eat_hash_string<W: Word, const VEC_LEN: usize>(
    mut src: *const u8,
    src_end: *const u8,
    bitmasks: [&[W]; NUM_VECS],
) -> *const u8 {
    unsafe {
        debug_assert_eq!(src.sub(1).read(), b'r');
        debug_assert_eq!(src.read(), b'#');

        let mut num_hashes = 0u32;
        while src.read() == b'#' {
            num_hashes += 1;
            src = src.add(1);
        }

        let b'"' = src.read() else { return src };
        src = src.add(1);

        'outer: loop {
            match src.read() {
                b'"' => {
                    src = src.add(1);
                    let mut num_hashes = num_hashes;
                    while src.read() == b'#' {
                        src = src.add(1);
                        num_hashes -= 1;
                        if num_hashes == 0 {
                            break 'outer;
                        }
                    }
                }
                EOF_BYTE => break,
                _ => src = src.add(1),
            }
        }
        src
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use expect_test::{Expect, expect};

    use super::*;

    const VEC_LEN: usize = 16;
    type W = u16;

    #[track_caller]
    fn check(src: &str, expect: &Expect) {
        let (input, mut bitmask_vecs) = prepare_input::<W, VEC_LEN>(src);
        stage1::<W, VEC_LEN>(&input, &mut bitmask_vecs);
        let bitmask_slices = bitmask_vecs.each_ref().map(Vec::as_slice);

        let mut out = vec![EOF_BYTE; input.len() * 10];
        let buf = stage2::<W, VEC_LEN>(&input, bitmask_slices, &mut out);
        let mut out_iter = buf.iter().copied();

        let mut pos = 0usize;
        let mut decoded = Vec::new();
        while let Some(kind) = out_iter.next() {
            if kind == EOF_BYTE {
                break;
            }
            let Some(kind) = TokenKind::from_u8(kind) else {
                panic!("Invalid token kind byte: {kind} ({kind:#04x})");
            };
            let len = match kind.is_punct() {
                true => 1,
                false => u32::from_ne_bytes(out_iter.next_chunk().expect("Expected length byte")),
            };
            let end = pos + len as usize;
            decoded.push((kind, pos..end));
            pos = end;
        }

        let mut out = String::new();
        for (kind, range) in decoded {
            let lexeme = &src[range.clone()];
            _ = writeln!(out, "({kind:?}, {range:?}, {lexeme:?})");
        }

        expect.assert_eq(&out);
    }

    #[test]
    fn empty() { check("", &expect![[""]]); }

    #[test]
    fn whitespace() {
        check(" ", &expect![[r#"
            (Whitespace, 0..1, " ")
        "#]]);
        check("\t", &expect![[r#"
            (Whitespace, 0..1, "\t")
        "#]]);
        check("\n", &expect![[r#"
            (Whitespace, 0..1, "\n")
        "#]]);
        check(" \t\n", &expect![[r#"
            (Whitespace, 0..3, " \t\n")
        "#]]);
        check(" \t\n \t\n \t\n \t\n \t\n", &expect![[r#"
            (Whitespace, 0..15, " \t\n \t\n \t\n \t\n \t\n")
        "#]]);
        check(" \t\n \t\n \t\n \t\n \t\n ", &expect![[r#"
            (Whitespace, 0..16, " \t\n \t\n \t\n \t\n \t\n ")
        "#]]);
        check(" \t\n \t\n \t\n \t\n \t\n          ", &expect![[r#"
            (Whitespace, 0..25, " \t\n \t\n \t\n \t\n \t\n          ")
        "#]]);
    }

    #[test]
    fn line_comments() {
        check("//hello\n", &expect![[r#"
            (LineComment, 0..7, "//hello")
            (Whitespace, 7..8, "\n")
        "#]]);
        check("//hello\n   ", &expect![[r#"
            (LineComment, 0..7, "//hello")
            (Whitespace, 7..11, "\n   ")
        "#]]);
        check("//hello", &expect![[r#"
            (LineComment, 0..7, "//hello")
        "#]]);
        check("// line comment newline\n//line comment EOF", &expect![[
            r#"
            (LineComment, 0..23, "// line comment newline")
            (Whitespace, 23..24, "\n")
            (LineComment, 24..42, "//line comment EOF")
        "#
        ]]);
    }

    #[test]
    fn block_comments1() {
        check("/* hello */", &expect![[r#"
        (BlockComment, 0..11, "/* hello */")
    "#]]);
    }

    #[test]
    fn block_comments2() {
        check("/* hello /* nested */", &expect![[r#"
        (BlockComment, 0..21, "/* hello /* nested */")
    "#]]);
    }

    #[test]
    fn block_comments3() {
        check("/* hello /* nested */ world */ goodbye", &expect![[r#"
            (BlockComment, 0..30, "/* hello /* nested */ world */")
            (Whitespace, 30..31, " ")
            (Ident, 31..38, "goodbye")
        "#]]);
    }

    #[test]
    fn block_comments4() {
        check(
            "_23456789abc/* /* unclosed block comment */
            oh no, still in a comment",
            &expect![[r#"
            (Ident, 0..12, "_23456789abc")
            (BlockComment, 12..81, "/* /* unclosed block comment */\n            oh no, still in a comment")
            "#]],
        );
    }

    #[test]
    fn block_comments5() {
        check(
            "/* block comment */
            /* nested block comment /* still nested */ also still nested */
            /* /* unclosed block comment */
            oh no, still in a comment",
            &expect![[r#"
            (BlockComment, 0..19, "/* block comment */")
            (Whitespace, 19..32, "\n            ")
            (BlockComment, 32..95, "/* nested block comment /* still nested */ also still nested */")
            (Whitespace, 95..108, "\n            ")
            (BlockComment, 108..177, "/* /* unclosed block comment */\n            oh no, still in a comment")
        "#]],
        );
    }

    #[test]
    fn block_comments6() {
        check("/* EOF", &expect![[r#"
            (BlockComment, 0..6, "/* EOF")
        "#]]);
        check("/*/ EOF", &expect![[r#"
            (BlockComment, 0..7, "/*/ EOF")
        "#]]);
        check("/**/ EOF", &expect![[r#"
            (BlockComment, 0..4, "/**/")
            (Whitespace, 4..5, " ")
            (Ident, 5..8, "EOF")
        "#]]);
        check("/*// EOF", &expect![[r#"
            (BlockComment, 0..8, "/*// EOF")
        "#]]);
        check("/*/* EOF", &expect![[r#"
            (BlockComment, 0..8, "/*/* EOF")
        "#]]);
        check("/*/**/ EOF", &expect![[r#"
            (BlockComment, 0..10, "/*/**/ EOF")
        "#]]);
        check("/* /* */ */ EOF", &expect![[r#"
            (BlockComment, 0..11, "/* /* */ */")
            (Whitespace, 11..12, " ")
            (Ident, 12..15, "EOF")
        "#]]);
        check("/*/* */ */ EOF", &expect![[r#"
            (BlockComment, 0..10, "/*/* */ */")
            (Whitespace, 10..11, " ")
            (Ident, 11..14, "EOF")
        "#]]);
        check("/*/* */*/ EOF", &expect![[r#"
            (BlockComment, 0..9, "/*/* */*/")
            (Whitespace, 9..10, " ")
            (Ident, 10..13, "EOF")
        "#]]);
        check("/*/**/*/ EOF", &expect![[r#"
            (BlockComment, 0..8, "/*/**/*/")
            (Whitespace, 8..9, " ")
            (Ident, 9..12, "EOF")
        "#]]);
        check("/**//**/ EOF", &expect![[r#"
            (BlockComment, 0..4, "/**/")
            (BlockComment, 4..8, "/**/")
            (Whitespace, 8..9, " ")
            (Ident, 9..12, "EOF")
        "#]]);
    }

    #[test]
    fn idents() {
        check("a", &expect![[r#"
            (Ident, 0..1, "a")
        "#]]);
        check("abc123", &expect![[r#"
            (Ident, 0..6, "abc123")
        "#]]);
        check("_", &expect![[r#"
            (Ident, 0..1, "_")
        "#]]);
        check("abc_123_", &expect![[r#"
            (Ident, 0..8, "abc_123_")
        "#]]);
        check("abcdef123456789", &expect![[r#"
            (Ident, 0..15, "abcdef123456789")
        "#]]);
        check("abcdef1234567890", &expect![[r#"
            (Ident, 0..16, "abcdef1234567890")
        "#]]);
        check("abcdef1234567890xyz", &expect![[r#"
            (Ident, 0..19, "abcdef1234567890xyz")
        "#]]);
    }

    #[test]
    fn idents_and_whitespace() {
        check("a b c", &expect![[r#"
            (Ident, 0..1, "a")
            (Whitespace, 1..2, " ")
            (Ident, 2..3, "b")
            (Whitespace, 3..4, " ")
            (Ident, 4..5, "c")
        "#]]);

        check("abc  def  ghi", &expect![[r#"
            (Ident, 0..3, "abc")
            (Whitespace, 3..5, "  ")
            (Ident, 5..8, "def")
            (Whitespace, 8..10, "  ")
            (Ident, 10..13, "ghi")
        "#]]);
    }

    #[test]
    fn punctuation() {
        check("!#$%&()*+,-./:;<=>?[]^{|}~", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (Dollar, 2..3, "$")
            (Percent, 3..4, "%")
            (Ampersand, 4..5, "&")
            (LParen, 5..6, "(")
            (RParen, 6..7, ")")
            (Star, 7..8, "*")
            (Plus, 8..9, "+")
            (Comma, 9..10, ",")
            (Minus, 10..11, "-")
            (Dot, 11..12, ".")
            (Slash, 12..13, "/")
            (Colon, 13..14, ":")
            (Semicolon, 14..15, ";")
            (Lt, 15..16, "<")
            (Eq, 16..17, "=")
            (Gt, 17..18, ">")
            (Question, 18..19, "?")
            (LSquare, 19..20, "[")
            (RSquare, 20..21, "]")
            (Caret, 21..22, "^")
            (LCurly, 22..23, "{")
            (Bar, 23..24, "|")
            (RCurly, 24..25, "}")
            (Tilde, 25..26, "~")
        "##]]);
    }

    #[test]
    fn punct2() {
        check("!#///*\n", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (LineComment, 2..6, "///*")
            (Whitespace, 6..7, "\n")
        "##]]);
        check("!#/*\n*/~>", &expect![[r##"
            (Bang, 0..1, "!")
            (Hash, 1..2, "#")
            (BlockComment, 2..7, "/*\n*/")
            (Tilde, 7..8, "~")
            (Gt, 8..9, ">")
        "##]]);
    }

    #[test]
    fn numbers() {
        check(
            "0 1234567890 123_456 123suffix 1.2 0.1 0. 0..1 0. 1e 1E 1e+ 1e- 1e+2 1e+2suffix",
            &expect![[r#"
                (Int, 0..1, "0")
                (Whitespace, 1..2, " ")
                (Int, 2..12, "1234567890")
                (Whitespace, 12..13, " ")
                (Int, 13..20, "123_456")
                (Whitespace, 20..21, " ")
                (Int, 21..30, "123suffix")
                (Whitespace, 30..31, " ")
                (Float, 31..34, "1.2")
                (Whitespace, 34..35, " ")
                (Float, 35..38, "0.1")
                (Whitespace, 38..39, " ")
                (Float, 39..41, "0.")
                (Whitespace, 41..42, " ")
                (Int, 42..43, "0")
                (Dot, 43..44, ".")
                (Dot, 44..45, ".")
                (Int, 45..46, "1")
                (Whitespace, 46..47, " ")
                (Float, 47..49, "0.")
                (Whitespace, 49..50, " ")
                (Float, 50..52, "1e")
                (Whitespace, 52..53, " ")
                (Float, 53..55, "1E")
                (Whitespace, 55..56, " ")
                (Float, 56..59, "1e+")
                (Whitespace, 59..60, " ")
                (Float, 60..63, "1e-")
                (Whitespace, 63..64, " ")
                (Float, 64..68, "1e+2")
                (Whitespace, 68..69, " ")
                (Float, 69..79, "1e+2suffix")
            "#]],
        );

        check(
            "0b10_1010asdfbz 0o755as_dfzxc 0xDEADBE_EFasdfzxc",
            &expect![[r#"
                (Int, 0..15, "0b10_1010asdfbz")
                (Whitespace, 15..16, " ")
                (Int, 16..29, "0o755as_dfzxc")
                (Whitespace, 29..30, " ")
                (Int, 30..48, "0xDEADBE_EFasdfzxc")
            "#]],
        );
    }

    #[test]
    fn chars() {
        check("'a' '\n' '\\'' '' 'foo'", &expect![[r#"
            (Char, 0..3, "'a'")
            (Whitespace, 3..4, " ")
            (Char, 4..7, "'\n'")
            (Whitespace, 7..8, " ")
            (Char, 8..12, "'\\''")
            (Whitespace, 12..13, " ")
            (Char, 13..15, "''")
            (Whitespace, 15..16, " ")
            (Char, 16..21, "'foo'")
        "#]]);
        check("b'a' b'\n' b'\\'' b'' b'foo'", &expect![[r#"
            (Byte, 0..4, "b'a'")
            (Whitespace, 4..5, " ")
            (Byte, 5..9, "b'\n'")
            (Whitespace, 9..10, " ")
            (Byte, 10..15, "b'\\''")
            (Whitespace, 15..16, " ")
            (Byte, 16..19, "b''")
            (Whitespace, 19..20, " ")
            (Byte, 20..26, "b'foo'")
        "#]]);
    }

    #[test]
    fn strings() {
        check(r#""" "simple" "escaped \" quote" "unterminated"#, &expect![
            [r#"
                (Str, 0..2, "\"\"")
                (Whitespace, 2..3, " ")
                (Str, 3..11, "\"simple\"")
                (Whitespace, 11..12, " ")
                (Str, 12..30, "\"escaped \\\" quote\"")
                (Whitespace, 30..31, " ")
                (Str, 31..44, "\"unterminated")
            "#]
        ]);
        check(
            r#"b"" b"simple" b"escaped \" quote" b"unterminated"#,
            &expect![[r#"
                (ByteStr, 0..3, "b\"\"")
                (Whitespace, 3..4, " ")
                (ByteStr, 4..13, "b\"simple\"")
                (Whitespace, 13..14, " ")
                (ByteStr, 14..33, "b\"escaped \\\" quote\"")
                (Whitespace, 33..34, " ")
                (ByteStr, 34..48, "b\"unterminated")
            "#]],
        );
        check(
            r#"c"" c"simple" c"escaped \" quote" c"unterminated"#,
            &expect![[r#"
                (CStr, 0..3, "c\"\"")
                (Whitespace, 3..4, " ")
                (CStr, 4..13, "c\"simple\"")
                (Whitespace, 13..14, " ")
                (CStr, 14..33, "c\"escaped \\\" quote\"")
                (Whitespace, 33..34, " ")
                (CStr, 34..48, "c\"unterminated")
            "#]],
        );
    }

    #[test]
    fn raw_strings() {
        check(
            r#"
    r"raw string\"
    br"raw string\"
    cr"raw string\"
    r"unterminated
    "#,
            &expect![[r#"
                (Whitespace, 0..5, "\n    ")
                (RawStr, 5..19, "r\"raw string\\\"")
                (Whitespace, 19..24, "\n    ")
                (RawByteStr, 24..39, "br\"raw string\\\"")
                (Whitespace, 39..44, "\n    ")
                (RawCStr, 44..59, "cr\"raw string\\\"")
                (Whitespace, 59..64, "\n    ")
                (RawStr, 64..83, "r\"unterminated\n    ")
            "#]],
        );
    }

    #[test]
    fn hash_strings() {
        check(
            r###"
            r#""#
            r##""##
            r#"raw string""""""""#
            r#"""""""""#
            r##" ##"" "##
            r#"unterminated" "###,
            &expect![[r###"
                (Whitespace, 0..13, "\n            ")
                (RawStr, 13..18, "r#\"\"#")
                (Whitespace, 18..31, "\n            ")
                (RawStr, 31..38, "r##\"\"##")
                (Whitespace, 38..51, "\n            ")
                (RawStr, 51..73, "r#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 73..86, "\n            ")
                (RawStr, 86..98, "r#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 98..111, "\n            ")
                (RawStr, 111..124, "r##\" ##\"\" \"##")
                (Whitespace, 124..137, "\n            ")
                (RawStr, 137..154, "r#\"unterminated\" ")
            "###]],
        );

        check(r#"r#""#, &expect![[r#"
            (RawStr, 0..3, "r#\"")
        "#]]);
        check(r#"r#"""#, &expect![[r#"
            (RawStr, 0..4, "r#\"\"")
        "#]]);
        check(
            r###"
    br#""#
    br##""##
    br#"raw string""""""""#
    br#"""""""""#
    br##" ##"" "##
    br#"unterminated"
    "###,
            &expect![[r###"
                (Whitespace, 0..5, "\n    ")
                (RawByteStr, 5..11, "br#\"\"#")
                (Whitespace, 11..16, "\n    ")
                (RawByteStr, 16..24, "br##\"\"##")
                (Whitespace, 24..29, "\n    ")
                (RawByteStr, 29..52, "br#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 52..57, "\n    ")
                (RawByteStr, 57..70, "br#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 70..75, "\n    ")
                (RawByteStr, 75..89, "br##\" ##\"\" \"##")
                (Whitespace, 89..94, "\n    ")
                (RawByteStr, 94..116, "br#\"unterminated\"\n    ")
            "###]],
        );

        check(
            r###"
    cr#""#
    cr##""##
    cr#"raw string""""""""#
    cr#"""""""""#
    cr##" ##"" "##
    cr#"unterminated"
    "###,
            &expect![[r###"
                (Whitespace, 0..5, "\n    ")
                (RawCStr, 5..11, "cr#\"\"#")
                (Whitespace, 11..16, "\n    ")
                (RawCStr, 16..24, "cr##\"\"##")
                (Whitespace, 24..29, "\n    ")
                (RawCStr, 29..52, "cr#\"raw string\"\"\"\"\"\"\"\"#")
                (Whitespace, 52..57, "\n    ")
                (RawCStr, 57..70, "cr#\"\"\"\"\"\"\"\"\"#")
                (Whitespace, 70..75, "\n    ")
                (RawCStr, 75..89, "cr##\" ##\"\" \"##")
                (Whitespace, 89..94, "\n    ")
                (RawCStr, 94..116, "cr#\"unterminated\"\n    ")
            "###]],
        );
    }
}
