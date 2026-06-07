#![allow(clippy::wildcard_imports)]

use std::bstr::ByteStr;
use std::debug_assert_matches;
use std::simd::Simd;

use crate::TokenKind;
use crate::utils::simdx::*;
use crate::utils::*;

pub const EOF_BYTE: u8 = 0xff;

pub fn prepare_input<const VEC_LEN: usize>(src: &str) -> (Vec<u8>, Vec<u32>) {
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

        let eof_pos = src.len() as u32;
        let output_vec = vec![eof_pos; input_vec.len()];
        (input_vec, output_vec)
    }
}

fn fmt_bitmask<const VEC_LEN: usize>(bits: u64) -> impl std::fmt::Display {
    std::fmt::from_fn(move |f| match VEC_LEN {
        16 => write!(f, "{:0VEC_LEN$b}", (bits as u16).reverse_bits()),
        32 => write!(f, "{:0VEC_LEN$b}", (bits as u32).reverse_bits()),
        64 => write!(f, "{:0VEC_LEN$b}", bits.reverse_bits()),
        _ => unreachable!(),
    })
}

/// Return a mask with 1s at each character which is escaped by an odd number of
/// backslashes.
fn escaped_chars_mask<const VEC_LEN: usize>(
    backslashes: u64,
    mut prev_escaped: bool,
) -> (u64, bool) {
    // ----------------------------------------------------------------------------
    // This code is brought to you courtesy of simdjson, licensed
    // under the Apache 2.0 license which is included at the bottom of this file
    // Credit to John Keiser (@jkeiser) for designing this algorithm.
    // See https://github.com/simdjson/simdjson/pull/2042

    const ODD_BITS: u64 = 0xA_AA_AA_AA_AA_AA_AA; // 1010 repeated
    // |                                | Mask (shows characters instead of 1's) | Depth | Instructions        |
    // |--------------------------------|----------------------------------------|-------|---------------------|
    // | string                         | `\\n_\\\n___\\\n___\\\\___\\\\__\\\`   |       |                     |
    // |                                | `    even   odd    even   odd   odd`   |       |                     |
    // | potential_escape               | ` \  \\\    \\\    \\\\   \\\\  \\\`   | 1     | 1 (backslashes & ~first_is_escaped)
    // | escape_and_terminal_code       | ` \n \ \n   \ \n   \ \    \ \   \ \`   | 5     | 5 (next_escape_and_terminal_code())
    // | escaped                        | `\    \ n    \ n    \ \    \ \   \ ` X | 6     | 7 (escape_and_terminal_code ^ (potential_escape | first_is_escaped))
    // | escape                         | `    \ \    \ \    \ \    \ \   \ \`   | 6     | 8 (escape_and_terminal_code & backslashes)
    // | first_is_escaped               | `\                                 `   | 7 (*) | 9 (escape >> 63) ()
    //
    // (*) this is not needed until the next iteration
    let potential_escape = backslashes & !u64::from(prev_escaped);

    // If we were to just shift and mask out any odd bits, we'd actually get a
    // *half* right answer: any even-aligned backslashes runs would be correct!
    // Odd-aligned backslashes runs would be inverted (\\\ would be 010 instead
    // of 101).
    //
    // ```
    // string:              | ____\\\\_\\\\_____ |
    // maybe_escaped | ODD  |     \ \   \ \      |
    //               even-aligned ^^^  ^^^^ odd-aligned
    // ```
    //
    // Taking that into account, our basic strategy is:
    //
    // 1. Use subtraction to produce a mask with 1's for even-aligned runs and 0's
    //    for odd-aligned runs.
    // 2. XOR all odd bits, which masks out the odd bits in even-aligned runs, and
    //    brings IN the odd bits in odd-aligned runs.
    // 3. & with backslashes to clean up any stray bits.
    // runs are set to 0, and then XORing with "odd":
    //
    // |                                | Mask (shows characters instead of 1's) | Instructions        |
    // |--------------------------------|----------------------------------------|---------------------|
    // | string                         | `\\n_\\\n___\\\n___\\\\___\\\\__\\\`   |
    // |                                | `    even   odd    even   odd   odd`   |
    // | maybe_escaped                  | `  n  \\n    \\n    \\\_   \\\_  \\` X | 1 (potential_escape << 1)
    // | maybe_escaped_and_odd          | ` \n_ \\n _ \\\n_ _ \\\__ _\\\_ \\\`   | 1 (maybe_escaped | odd)
    // | even_series_codes_and_odd      | `  n_\\\  _    n_ _\\\\ _     _    `   | 1 (maybe_escaped_and_odd - potential_escape)
    // | escape_and_terminal_code       | ` \n \ \n   \ \n   \ \    \ \   \ \`   | 1 (^ odd)
    //

    // Escaped characters are characters following an escape.
    let maybe_escaped = potential_escape << 1;

    // To distinguish odd from even escape sequences, therefore, we turn on any
    // *starting* escapes that are on an odd byte. (We actually bring in all odd
    // bits, for speed.)
    // - Odd runs of backslashes are 0000, and the code at the end ("n" in \n or
    //   \\n) is 1.
    // - Odd runs of backslashes are 1111, and the code at the end ("n" in \n or
    //   \\n) is 0.
    // - All other odd bytes are 1, and even bytes are 0.
    let even_series_codes_and_odd_bits = (maybe_escaped | ODD_BITS) - potential_escape;

    // Now we flip all odd bytes back with xor. This:
    // - Makes odd runs of backslashes go from 0000 to 1010
    // - Makes even runs of backslashes go from 1111 to 1010
    // - Sets actually-escaped codes to 1 (the n in \n and \\n: \n = 11, \\n = 100)
    // - Resets all other bytes to 0
    let escape_and_terminal_code = even_series_codes_and_odd_bits ^ ODD_BITS;
    let escaped = escape_and_terminal_code ^ (backslashes | u64::from(prev_escaped));
    prev_escaped = (escape_and_terminal_code & backslashes) >> (VEC_LEN - 1) != 0;
    (escaped, prev_escaped)
}

fn idents_mask<const VEC_LEN: usize>(id_chars: u64, carry: &mut Carry) -> u64 {
    debug_assert_matches!(carry.ident, 1 | 0);

    let id_starts = id_chars & !(id_chars << 1) & !carry.ident;
    carry.ident = id_chars >> (VEC_LEN - 1);
    debug_assert_matches!(carry.ident, 1 | 0);

    id_starts
}

fn whitespace_mask<const VEC_LEN: usize>(ws_chars: u64, carry: &mut Carry) -> u64 {
    debug_assert_matches!(carry.whitespace, 1 | 0);

    let ws_starts = ws_chars & !(ws_chars << 1) & !carry.whitespace;
    carry.whitespace = ws_chars >> (VEC_LEN - 1);
    debug_assert_matches!(carry.whitespace, 1 | 0);

    ws_starts
}

#[inline(always)]
unsafe fn write_indices<const VEC_LEN: usize>(mut out_ptr: *mut u32, mut mask: u64, idx: u32) {
    let ceil = VEC_LEN as u32;

    unsafe {
        if cfg!(target_arch = "aarch64") {
            mask = mask.reverse_bits();
            while mask != 0 {
                unroll!(16, {
                    out_ptr = write_and_advance(out_ptr, idx + mask.leading_zeros().min(ceil));
                    mask &= !mask.isolate_highest_one();
                });
            }
        } else {
            while mask != 0 {
                unroll!(16, {
                    out_ptr = write_and_advance(out_ptr, idx + mask.trailing_zeros().min(ceil));
                    mask &= !mask.isolate_lowest_one();
                });
            }
        }
    }
}

#[derive(Default)]
#[allow(clippy::struct_excessive_bools)]
struct Carry {
    whitespace: u64,
    ident:      u64,
    slash:      u64,
}

struct Masks<const VEC_LEN: usize> {
    whitespace:            u64,
    idents:                u64,
    puncts:                u64,
    quotes_or_apostrophes: u64,
    line_comments:         u64,
    block_comments:        u64,
}
impl<const VEC_LEN: usize> Masks<VEC_LEN> {
    fn normals(&self) -> u64 {
        let mask = self.whitespace | self.idents | self.puncts;
        match VEC_LEN {
            16 => mask & 0xFFFF,
            32 => mask & 0xFFFF_FFFF,
            64 => mask,
            _ => unreachable!(),
        }
    }
    fn specials(&self) -> u64 {
        self.quotes_or_apostrophes | self.line_comments | self.block_comments
    }
}

fn get_mask<const VEC_LEN: usize>(vec: Simd<u8, VEC_LEN>, carry: &mut Carry) -> Masks<VEC_LEN> {
    let ws_chars = movemask(in_range(vec, 0x09, 0x0D) | eq(vec, b' '));
    let ident_chars = movemask(
        eq(vec, b'_')
            | in_range(vec, b'a', b'z')
            | in_range(vec, b'A', b'Z')
            | in_range(vec, b'0', b'9'),
    );

    deprintln!("vec           = {}", ByteStr::new(vec.as_array()));
    deprintln!("ws_chars      = {}", fmt_bitmask::<VEC_LEN>(ws_chars));
    deprintln!("ident_chars   = {}", fmt_bitmask::<VEC_LEN>(ident_chars));

    let whitespace = whitespace_mask::<VEC_LEN>(ws_chars, carry);
    deprintln!("ws_starts     = {}", fmt_bitmask::<VEC_LEN>(whitespace));

    let idents = idents_mask::<VEC_LEN>(ident_chars, carry);
    deprintln!("ident_starts  = {}", fmt_bitmask::<VEC_LEN>(idents));

    // Any character that is not an alphanumeric char or a whitespace char is a
    // punctuation char.
    let puncts = !ws_chars & !ident_chars;
    deprintln!("punct         = {}", fmt_bitmask::<VEC_LEN>(puncts));

    let normal_starts = idents | puncts | whitespace;
    deprintln!("normal_starts = {}", fmt_bitmask::<VEC_LEN>(normal_starts));
    deprintln!("vec           = {}", ByteStr::new(vec.as_array()));

    let quotes_or_apostrophes = movemask(eq(vec, b'"') | eq(vec, b'\''));
    let slash = movemask(eq(vec, b'/'));
    let star = movemask(eq(vec, b'*'));

    let next_slash = slash >> 1;
    let next_star = star >> 1;

    let line_comments = (carry.slash | (slash << 1)) & slash; // `//`
    let block_comments = (carry.slash | (slash << 1)) & star; // `/*`

    let specials = quotes_or_apostrophes | line_comments | block_comments;

    deprintln!();
    deprintln!("vec      = {}", ByteStr::new(&vec));
    // deprintln!("\"        = {}", fmt_bitmask::<VEC_LEN>(quotes));
    // deprintln!("'        = {}", fmt_bitmask::<VEC_LEN>(apostrophes));
    deprintln!("*        = {}", fmt_bitmask::<VEC_LEN>(star));
    deprintln!("* >> 1   = {}", fmt_bitmask::<VEC_LEN>(next_star));
    deprintln!("/ carry  = {}", fmt_bitmask::<VEC_LEN>(carry.slash));
    deprintln!("/        = {}", fmt_bitmask::<VEC_LEN>(slash));
    deprintln!("/ >> 1   = {}", fmt_bitmask::<VEC_LEN>(next_slash));
    deprintln!("//       = {}", fmt_bitmask::<VEC_LEN>(line_comments));
    deprintln!("/*       = {}", fmt_bitmask::<VEC_LEN>(block_comments));
    deprintln!("specials = {}", fmt_bitmask::<VEC_LEN>(specials));
    deprintln!();

    carry.slash = slash >> (VEC_LEN - 1);

    Masks {
        whitespace,
        idents,
        puncts,
        quotes_or_apostrophes,
        line_comments,
        block_comments,
    }
}

pub fn stage1<'a, const VEC_LEN: usize>(
    padded_src: &[u8],
    out_slice: &'a mut [u32],
) -> &'a mut [u32] {
    unsafe {
        debug_assert!(out_slice.len() >= padded_src.len());
        let src = padded_src.strip_suffix(&[EOF_BYTE; VEC_LEN]).unwrap();
        let src = src.strip_suffix(&[EOF_BYTE; VEC_LEN]).unwrap();

        let src_start = src.as_ptr();
        let src_end = src.as_ptr_range().end;
        let mut src_ptr = src_start;
        let mut out_ptr = out_slice.as_mut_ptr();

        let mut idx = 0;
        let mut carry = Carry::default();
        'outer: loop {
            let vec1 = load::<VEC_LEN>(src_ptr);
            let mask1 = get_mask(vec1, &mut carry);
            let normals = mask1.normals();
            let specials = mask1.specials();

            if specials == 0 {
                write_indices::<VEC_LEN>(out_ptr, normals, idx);
                out_ptr = out_ptr.add(normals.count_ones() as usize);
            } else {
                let first_special = specials.isolate_lowest_one();
                let normals = normals & (first_special - 1);
                write_indices::<VEC_LEN>(out_ptr, normals, idx);
                out_ptr = out_ptr.add(normals.count_ones() as usize);

                let spec_tz = specials.trailing_zeros();
                let token_start_pos = idx + spec_tz;
                let token_start_ptr = src_ptr.add(spec_tz as usize);
                let mut token_end_ptr = token_start_ptr.add(1);

                let prev_byte = token_start_ptr.sub(1).read();
                let byte = token_start_ptr.read();
                match byte {
                    b'"' if prev_byte == b'r' => {
                        out_ptr = write_and_advance(out_ptr, token_start_pos);
                        loop {
                            match token_end_ptr.read() {
                                b'"' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    break;
                                }
                                EOF_BYTE => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                    }
                    b'"' if prev_byte == b'#' => {
                        out_ptr = write_and_advance(out_ptr, token_start_pos);
                        let num_hashes = {
                            let mut n = 0u32;
                            let mut ptr = token_start_ptr.sub(1);
                            while ptr.read() == b'#' {
                                n += 1;
                                ptr = ptr.sub(1);
                            }
                            n
                        };

                        'foo: loop {
                            match token_end_ptr.read() {
                                b'"' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    let mut hashes = num_hashes;
                                    while token_end_ptr.read() == b'#' {
                                        hashes -= 1;
                                        token_end_ptr = token_end_ptr.add(1);
                                        if hashes == 0 {
                                            break 'foo;
                                        }
                                    }
                                }
                                EOF_BYTE => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                    }
                    b'"' => {
                        out_ptr = write_and_advance(out_ptr, token_start_pos);
                        loop {
                            match token_end_ptr.read() {
                                b'"' => {
                                    token_end_ptr = token_end_ptr.add(1);
                                    break;
                                }
                                EOF_BYTE => break,
                                b'\\' => token_end_ptr = token_end_ptr.add(2),
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                    }
                    b'\'' => {
                        out_ptr = write_and_advance(out_ptr, token_start_pos);
                        if let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' = token_end_ptr.read()
                        {
                            while let b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' =
                                token_end_ptr.read()
                            {
                                token_end_ptr = token_end_ptr.add(1);
                            }
                            if token_end_ptr.read() == b'\'' {
                                token_end_ptr = token_end_ptr.add(1);
                            }
                        } else {
                            loop {
                                match token_end_ptr.read() {
                                    b'\'' => {
                                        token_end_ptr = token_end_ptr.add(1);
                                        break;
                                    }
                                    EOF_BYTE => break,
                                    b'\\' => token_end_ptr = token_end_ptr.add(2),
                                    _ => token_end_ptr = token_end_ptr.add(1),
                                }
                            }
                        }
                    }
                    b'/' if prev_byte == b'/' => {
                        let mut src_ptr = token_end_ptr;
                        loop {
                            let vec = load::<VEC_LEN>(src_ptr);
                            let newlines = movemask(eq(vec, b'\n'));
                            let tz = newlines.trailing_zeros().min(VEC_LEN as u32);
                            if tz != VEC_LEN as u32 {
                                token_end_ptr = src_ptr.add(tz as usize);
                                break;
                            }
                            src_ptr = src_ptr.add(VEC_LEN);
                            if src_ptr >= src_end {
                                token_end_ptr = src_end;
                                break;
                            }
                        }
                    }
                    b'*' if prev_byte == b'/' => {
                        let mut depth = 1;
                        loop {
                            match &token_end_ptr.cast::<[u8; 2]>().read() {
                                b"/*" => {
                                    token_end_ptr = token_end_ptr.add(2);
                                    depth += 1;
                                }
                                b"*/" => {
                                    token_end_ptr = token_end_ptr.add(2);
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                [EOF_BYTE, _] => break,
                                _ => token_end_ptr = token_end_ptr.add(1),
                            }
                        }
                    }
                    _ => unreachable!(
                        "unknown token start: {}",
                        ByteStr::new(&token_start_ptr.cast::<[u8; 32]>().read())
                    ),
                }

                let token_end_pos = token_end_ptr.offset_from_unsigned(src_start) as u32;
                src_ptr = token_end_ptr;
                idx = token_end_pos;
                carry = Carry::default();
                continue 'outer;
            }
            idx += VEC_LEN as u32;
            src_ptr = src_ptr.add(VEC_LEN);
            if src_ptr >= src_end {
                break;
            }
        }

        let len = src.len() as u32;
        out_ptr = write_and_advance(out_ptr, len);

        let out_len = out_ptr.offset_from_unsigned(out_slice.as_mut_ptr());
        &mut out_slice[..out_len]
    }
}

pub fn stage2<const VEC_LEN: usize>(src: &[u8], indexes: &[u32]) -> Vec<(TokenKind, u32, u32)> {
    let mut tokens = Vec::with_capacity(indexes.len());
    let mut index_iter = indexes.iter().copied();
    let Some(mut start) = index_iter.next() else {
        return tokens;
    };
    loop {
        match src[start as usize] {
            b'/' if src[start as usize + 1] == b'/' => {
                let end = index_iter.next().unwrap();
                debug_assert_matches!(
                    src[end as usize],
                    b'\n' | EOF_BYTE,
                    "unknown token: {}\ncontext: {}\nstart = {start}, end = {end}\nindexes = {:?}",
                    ByteStr::new(&src[start as usize..end as usize]),
                    ByteStr::new(&src[start as usize - 1..end as usize + 16]),
                    index_iter.clone().take(10).collect::<Vec<_>>(),
                );
                tokens.push((TokenKind::LineComment, start, end));
                start = end;
            }
            b'/' if src[start as usize + 1] == b'*' => {
                let close = index_iter.next().unwrap();
                tokens.push((TokenKind::BlockComment, start, close));
                start = close;
            }

            b @ (b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-'
            | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.'
            | b'!' | b'>' | b'<' | b'^' | b'/') => {
                let end = index_iter.next().unwrap();
                let kind = TokenKind::from_u8(b).unwrap();
                debug_assert_eq!(
                    end,
                    start + 1,
                    "b = {:?}, start = {start}, end = {end}",
                    b as char
                );
                tokens.push((kind, start, end));
                start = end;
            }
            b' ' | b'\t' | b'\n' | b'\r' => {
                let end = index_iter.next().unwrap();
                tokens.push((TokenKind::Whitespace, start, end));
                start = end;
            }
            b'"' => {
                let exclusive_end = index_iter.next().unwrap();
                tokens.push((TokenKind::Str, start, exclusive_end));
                start = exclusive_end;
            }
            b'b' => match src[start as usize + 1..] {
                [b'r', b'#', ..] => {
                    let mut hash_pos = start + 2;
                    while src[hash_pos as usize] == b'#' {
                        let hash = index_iter.next().unwrap();
                        debug_assert_eq!(hash, hash_pos);
                        hash_pos += 1;
                    }
                    let _quote = index_iter.next().unwrap();
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawByteStr, start, end));
                    start = end;
                }
                [b'r', b'"', ..] => {
                    let open_quote = index_iter.next().unwrap();
                    debug_assert_eq!(
                        open_quote,
                        start + 2,
                        "start = {start}, open_quote = {open_quote}"
                    );
                    let exclusive_end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawByteStr, start, exclusive_end));
                    start = exclusive_end;
                }
                [b'"', ..] => {
                    let open_quote = index_iter.next().unwrap();
                    debug_assert_eq!(
                        open_quote,
                        start + 1,
                        "start = {start}, open_quote = {open_quote}"
                    );
                    let exclusive_end = index_iter.next().unwrap();
                    tokens.push((TokenKind::ByteStr, start, exclusive_end));
                    start = exclusive_end;
                }
                [b'\'', ..] => {
                    let open_quote = index_iter.next().unwrap();
                    debug_assert_eq!(
                        open_quote,
                        start + 1,
                        "start = {start}, open_quote = {open_quote}"
                    );
                    let exclusive_end = index_iter.next().unwrap();
                    tokens.push((TokenKind::Byte, start, exclusive_end));
                    start = exclusive_end;
                }
                _ => {
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::Ident, start, end));
                    start = end;
                }
            },
            b'c' => match src[start as usize + 1..] {
                [b'r', b'#', ..] => {
                    let mut hash_pos = start + 2;
                    while src[hash_pos as usize] == b'#' {
                        let hash = index_iter.next().unwrap();
                        debug_assert_eq!(hash, hash_pos);
                        hash_pos += 1;
                    }
                    let _quote = index_iter.next().unwrap();
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawCStr, start, end));
                    start = end;
                }
                [b'r', b'"', ..] => {
                    let open_quote = index_iter.next().unwrap();
                    debug_assert_eq!(
                        open_quote,
                        start + 2,
                        "start = {start}, open_quote = {open_quote}"
                    );
                    let exclusive_end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawCStr, start, exclusive_end));
                    start = exclusive_end;
                }
                [b'"', ..] => {
                    let open_quote = index_iter.next().unwrap();
                    debug_assert_eq!(
                        open_quote,
                        start + 1,
                        "start = {start}, open_quote = {open_quote}"
                    );
                    let exclusive_end = index_iter.next().unwrap();
                    tokens.push((TokenKind::CStr, start, exclusive_end));
                    start = exclusive_end;
                }

                _ => {
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::Ident, start, end));
                    start = end;
                }
            },
            b'r' => match src[start as usize + 1..] {
                [b'#', b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_', ..] => {
                    let hash = index_iter.next().unwrap();
                    debug_assert_eq!(hash, start + 1, "start = {start}, hash = {hash}");
                    let ident_start = index_iter.next().unwrap();
                    debug_assert_eq!(
                        ident_start,
                        start + 2,
                        "start = {start}, ident_start = {ident_start}"
                    );
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawIdent, start, end));
                    start = end;
                }
                [b'#', ..] => {
                    let mut hash_pos = start + 1;
                    while src[hash_pos as usize] == b'#' {
                        let hash = index_iter.next().unwrap();
                        debug_assert_eq!(hash, hash_pos);
                        hash_pos += 1;
                    }
                    let _quote = index_iter.next().unwrap();
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawStr, start, end));
                    start = end;
                }
                [b'"', ..] => {
                    let open_quote = index_iter.next().unwrap();
                    debug_assert_eq!(
                        open_quote,
                        start + 1,
                        "start = {start}, open_quote = {open_quote}"
                    );
                    let exclusive_end = index_iter.next().unwrap();
                    tokens.push((TokenKind::RawStr, start, exclusive_end));
                    start = exclusive_end;
                }
                _ => {
                    let end = index_iter.next().unwrap();
                    tokens.push((TokenKind::Ident, start, end));
                    start = end;
                }
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let end = index_iter.next().unwrap();
                tokens.push((TokenKind::Ident, start, end));
                start = end;
            }
            b'0'..=b'9' => {
                let decimal_end = index_iter.next().unwrap();
                match &src[decimal_end as usize - 1..] {
                    [_, b'.', b'.' | b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => {
                        tokens.push((TokenKind::Int, start, decimal_end));
                        start = decimal_end;
                    }
                    [_, b'.', b'0'..=b'9', ..] => {
                        let _dot = index_iter.next().unwrap();
                        let decimal_end = index_iter.next().unwrap();
                        match src[decimal_end as usize - 1..] {
                            [b'e' | b'E', b'-' | b'+', ..] => {
                                let _sign = index_iter.next().unwrap();
                                let decimal_end = index_iter.next().unwrap();
                                tokens.push((TokenKind::Float, start, decimal_end));
                                start = decimal_end;
                            }
                            _ => {
                                tokens.push((TokenKind::Float, start, decimal_end));
                                start = decimal_end;
                            }
                        }
                    }
                    [b'e' | b'E', b'-' | b'+', ..] => {
                        let _sign = index_iter.next().unwrap();
                        let decimal_end = index_iter.next().unwrap();
                        tokens.push((TokenKind::Float, start, decimal_end));
                        start = decimal_end;
                    }
                    [_, b'.', ..] => {
                        let end = index_iter.next().unwrap();
                        tokens.push((TokenKind::Float, start, end));
                        start = end;
                    }
                    _ => {
                        tokens.push((TokenKind::Int, start, decimal_end));
                        start = decimal_end;
                    }
                }
            }
            b'\'' => {
                let end = index_iter.next().unwrap();
                match src[end as usize - 1] {
                    b'\'' => tokens.push((TokenKind::Char, start, end)),
                    _ => tokens.push((TokenKind::Lifetime, start, end)),
                }
                start = end;
            }
            EOF_BYTE => break,
            _ => {
                let end = index_iter.next().unwrap();
                tokens.push((TokenKind::Unknown, start, end));
                start = end;
            }
        }
    }
    tokens
}

pub fn stage1_16<'a>(src: &[u8], out_slice: &'a mut [u32]) -> &'a mut [u32] {
    stage1::<16>(src, out_slice)
}

pub fn stage1_32<'a>(src: &[u8], out_slice: &'a mut [u32]) -> &'a mut [u32] {
    stage1::<32>(src, out_slice)
}

pub fn stage1_64<'a>(src: &[u8], out_slice: &'a mut [u32]) -> &'a mut [u32] {
    stage1::<64>(src, out_slice)
}

#[cfg(test)]
mod stage1_tests {
    use std::bstr::ByteStr;

    use expect_test::{Expect, expect};

    use super::*;

    const VEC_LEN: usize = 16;

    #[track_caller]
    fn check(src: &str, expect: &Expect) {
        use std::fmt::Write;

        let (input_vec, mut output_vec) = prepare_input::<VEC_LEN>(src);
        let out_slice = stage1::<VEC_LEN>(&input_vec, &mut output_vec);
        let mut out_str = String::new();
        for idx in out_slice {
            _ = writeln!(
                out_str,
                "({idx}, «{}»)",
                ByteStr::new(&[input_vec[*idx as usize]])
            );
        }
        expect.assert_eq(&out_str);
    }

    #[test]
    fn empty() {
        check("", &expect![[r"
            (0, «�»)
            (1, «�»)
            (2, «�»)
            (3, «�»)
            (4, «�»)
            (5, «�»)
            (6, «�»)
            (7, «�»)
            (8, «�»)
            (9, «�»)
            (10, «�»)
            (11, «�»)
            (12, «�»)
            (13, «�»)
            (14, «�»)
            (15, «�»)
        "]]);
    }

    #[test]
    fn idents() {
        check("hello world", &expect![[r"
            (0, «h»)
            (5, « »)
            (6, «w»)
            (11, «�»)
            (12, «�»)
            (13, «�»)
            (14, «�»)
            (15, «�»)
        "]]);
        check("  hello  world  ", &expect![[r"
            (0, « »)
            (2, «h»)
            (7, « »)
            (9, «w»)
            (14, « »)
        "]]);

        check("  hello+world  ", &expect![[r"
            (0, « »)
            (2, «h»)
            (7, «+»)
            (8, «w»)
            (13, « »)
            (15, «�»)
        "]]);

        check("  hello + world  ", &expect![[r"
            (0, « »)
            (2, «h»)
            (7, « »)
            (8, «+»)
            (9, « »)
            (10, «w»)
            (15, « »)
            (17, «�»)
            (18, «�»)
            (19, «�»)
            (20, «�»)
            (21, «�»)
            (22, «�»)
            (23, «�»)
            (24, «�»)
            (25, «�»)
            (26, «�»)
            (27, «�»)
            (28, «�»)
            (29, «�»)
            (30, «�»)
            (31, «�»)
        "]]);
    }

    #[test]
    fn punctuation() {
        check("!#$%&()*+,-./:;<=>?[]^{|}~", &expect![[r"
            (0, «!»)
            (1, «#»)
            (2, «$»)
            (3, «%»)
            (4, «&»)
            (5, «(»)
            (6, «)»)
            (7, «*»)
            (8, «+»)
            (9, «,»)
            (10, «-»)
            (11, «.»)
            (12, «/»)
            (13, «:»)
            (14, «;»)
            (15, «<»)
            (16, «=»)
            (17, «>»)
            (18, «?»)
            (19, «[»)
            (20, «]»)
            (21, «^»)
            (22, «{»)
            (23, «|»)
            (24, «}»)
            (25, «~»)
            (26, «�»)
            (27, «�»)
            (28, «�»)
            (29, «�»)
            (30, «�»)
            (31, «�»)
        "]]);
        check("!#///*\n", &expect![[r"
            (0, «!»)
            (1, «#»)
            (2, «/»)
            (3, «/»)
            (4, «/»)
            (5, «*»)
            (6, «
            »)
            (7, «�»)
            (8, «�»)
            (9, «�»)
            (10, «�»)
            (11, «�»)
            (12, «�»)
            (13, «�»)
            (14, «�»)
            (15, «�»)
        "]]);
        check("!#/*\n*/~>", &expect![[r"
            (0, «!»)
            (1, «#»)
            (2, «/»)
            (3, «*»)
            (4, «
            »)
            (5, «*»)
            (6, «/»)
            (7, «~»)
            (8, «>»)
            (9, «�»)
            (10, «�»)
            (11, «�»)
            (12, «�»)
            (13, «�»)
            (14, «�»)
            (15, «�»)
        "]]);
    }

    #[test]
    fn strings() {
        check(r#""""#, &expect![[r#"
            (0, «"»)
            (2, «�»)
            (3, «�»)
            (4, «�»)
            (5, «�»)
            (6, «�»)
            (7, «�»)
            (8, «�»)
            (9, «�»)
            (10, «�»)
            (11, «�»)
            (12, «�»)
            (13, «�»)
            (14, «�»)
            (15, «�»)
        "#]]);

        check(r#"""#, &expect![[r#"
            (0, «"»)
        "#]]);

        check(r#""123456789abcdef"#, &expect![[r#"
            (0, «"»)
        "#]]);
        check(r#""123456789abcdef0"#, &expect![[r#"
            (0, «"»)
        "#]]);

        check(
            r#""" "simple" "escaped \" quote" "unterminated"#,
            //    01234567890123456789012345678901234567890123456
            &expect!([r#"
                (0, «"»)
                (2, « »)
                (3, «"»)
                (11, « »)
                (12, «"»)
                (30, « »)
                (31, «"»)
            "#]),
        );
    }
}

#[cfg(test)]
mod stage2_tests {

    use std::bstr::ByteStr;

    use expect_test::{Expect, expect};

    use super::*;

    const VEC_LEN: usize = 16;

    fn check(src: &str, expect: &Expect) {
        use std::fmt::Write;

        let (input_vec, mut index_vec) = prepare_input::<VEC_LEN>(src);
        let indexes = stage1::<VEC_LEN>(&input_vec, &mut index_vec);
        let tokens = stage2::<VEC_LEN>(&input_vec, indexes);
        let mut out_str = String::new();
        for (kind, start, end) in tokens {
            let lexeme = ByteStr::new(&input_vec[start as usize..end as usize]);
            _ = writeln!(out_str, "({kind:?}, {start}..{end}, «{lexeme}»)");
        }
        expect.assert_eq(&out_str);
    }

    #[test]
    fn empty() { check("", &expect![[""]]); }

    #[test]
    fn idents() {
        check("a", &expect![[r"
            (Ident, 0..1, «a»)
        "]]);
        check("abc123", &expect![[r"
            (Ident, 0..6, «abc123»)
        "]]);
        check("_", &expect![[r"
            (Ident, 0..1, «_»)
        "]]);
        check("abc_123_", &expect![[r"
            (Ident, 0..8, «abc_123_»)
        "]]);
        check("abcdef123456789", &expect![[r"
            (Ident, 0..15, «abcdef123456789»)
        "]]);

        check("abcdef1234567890", &expect![[r"
            (Ident, 0..16, «abcdef1234567890»)
        "]]);
    }

    #[test]
    fn idents2() {
        check("abcdef1234567890xyz", &expect![[r"
            (Ident, 0..19, «abcdef1234567890xyz»)
        "]]);
    }

    #[test]
    fn idents_and_whitespace() {
        check("a b c", &expect![[r"
            (Ident, 0..1, «a»)
            (Whitespace, 1..2, « »)
            (Ident, 2..3, «b»)
            (Whitespace, 3..4, « »)
            (Ident, 4..5, «c»)
        "]]);

        check("abc  def  ghi", &expect![[r"
            (Ident, 0..3, «abc»)
            (Whitespace, 3..5, «  »)
            (Ident, 5..8, «def»)
            (Whitespace, 8..10, «  »)
            (Ident, 10..13, «ghi»)
        "]]);
    }

    #[test]
    fn punctuation() {
        check("!#$%&()*+,-./:;<=>?[]^{|}~", &expect![[r"
            (Bang, 0..1, «!»)
            (Hash, 1..2, «#»)
            (Dollar, 2..3, «$»)
            (Percent, 3..4, «%»)
            (Ampersand, 4..5, «&»)
            (LParen, 5..6, «(»)
            (RParen, 6..7, «)»)
            (Star, 7..8, «*»)
            (Plus, 8..9, «+»)
            (Comma, 9..10, «,»)
            (Minus, 10..11, «-»)
            (Dot, 11..12, «.»)
            (Slash, 12..13, «/»)
            (Colon, 13..14, «:»)
            (Semicolon, 14..15, «;»)
            (Lt, 15..16, «<»)
            (Eq, 16..17, «=»)
            (Gt, 17..18, «>»)
            (Question, 18..19, «?»)
            (LSquare, 19..20, «[»)
            (RSquare, 20..21, «]»)
            (Caret, 21..22, «^»)
            (LCurly, 22..23, «{»)
            (Bar, 23..24, «|»)
            (RCurly, 24..25, «}»)
            (Tilde, 25..26, «~»)
        "]]);
        check("!#///*\n", &expect![[r"
            (Bang, 0..1, «!»)
            (Hash, 1..2, «#»)
            (LineComment, 2..6, «///*»)
            (Whitespace, 6..7, «
            »)
        "]]);
        check("!#/*\n*/~>", &expect![[r"
            (Bang, 0..1, «!»)
            (Hash, 1..2, «#»)
            (BlockComment, 2..7, «/*
            */»)
            (Tilde, 7..8, «~»)
            (Gt, 8..9, «>»)
        "]]);
    }

    #[test]
    fn strings() {
        check(
            r#"{ "\\\"Nam[": [ 116,"\\\\" , 234 , "true" , false ] , "t" : "\\\"" }"#,
            &expect![[r#"
                (LCurly, 0..1, «{»)
                (Whitespace, 1..2, « »)
                (Str, 2..12, «"\\\"Nam["»)
                (Colon, 12..13, «:»)
                (Whitespace, 13..14, « »)
                (LSquare, 14..15, «[»)
                (Whitespace, 15..16, « »)
                (Int, 16..19, «116»)
                (Comma, 19..20, «,»)
                (Str, 20..26, «"\\\\"»)
                (Whitespace, 26..27, « »)
                (Comma, 27..28, «,»)
                (Whitespace, 28..29, « »)
                (Int, 29..32, «234»)
                (Whitespace, 32..33, « »)
                (Comma, 33..34, «,»)
                (Whitespace, 34..35, « »)
                (Str, 35..41, «"true"»)
                (Whitespace, 41..42, « »)
                (Comma, 42..43, «,»)
                (Whitespace, 43..44, « »)
                (Ident, 44..49, «false»)
                (Whitespace, 49..50, « »)
                (RSquare, 50..51, «]»)
                (Whitespace, 51..52, « »)
                (Comma, 52..53, «,»)
                (Whitespace, 53..54, « »)
                (Str, 54..57, «"t"»)
                (Whitespace, 57..58, « »)
                (Colon, 58..59, «:»)
                (Whitespace, 59..60, « »)
                (Str, 60..66, «"\\\""»)
                (Whitespace, 66..67, « »)
                (RCurly, 67..68, «}»)
            "#]],
        );

        check(
            r#"{ b"\\\"Nam[": [ 116,b"\\\\" , 234 , b"true" , false ] , b"t" : b"\\\"" }"#,
            &expect![[r#"
                (LCurly, 0..1, «{»)
                (Whitespace, 1..2, « »)
                (ByteStr, 2..13, «b"\\\"Nam["»)
                (Colon, 13..14, «:»)
                (Whitespace, 14..15, « »)
                (LSquare, 15..16, «[»)
                (Whitespace, 16..17, « »)
                (Int, 17..20, «116»)
                (Comma, 20..21, «,»)
                (ByteStr, 21..28, «b"\\\\"»)
                (Whitespace, 28..29, « »)
                (Comma, 29..30, «,»)
                (Whitespace, 30..31, « »)
                (Int, 31..34, «234»)
                (Whitespace, 34..35, « »)
                (Comma, 35..36, «,»)
                (Whitespace, 36..37, « »)
                (ByteStr, 37..44, «b"true"»)
                (Whitespace, 44..45, « »)
                (Comma, 45..46, «,»)
                (Whitespace, 46..47, « »)
                (Ident, 47..52, «false»)
                (Whitespace, 52..53, « »)
                (RSquare, 53..54, «]»)
                (Whitespace, 54..55, « »)
                (Comma, 55..56, «,»)
                (Whitespace, 56..57, « »)
                (ByteStr, 57..61, «b"t"»)
                (Whitespace, 61..62, « »)
                (Colon, 62..63, «:»)
                (Whitespace, 63..64, « »)
                (ByteStr, 64..71, «b"\\\""»)
                (Whitespace, 71..72, « »)
                (RCurly, 72..73, «}»)
            "#]],
        );

        check(
            r#"{ c"\\\"Nam[": [ 116,c"\\\\" , 234 , c"true" , false ] , c"t" : c"\\\"" }"#,
            &expect![[r#"
                (LCurly, 0..1, «{»)
                (Whitespace, 1..2, « »)
                (CStr, 2..13, «c"\\\"Nam["»)
                (Colon, 13..14, «:»)
                (Whitespace, 14..15, « »)
                (LSquare, 15..16, «[»)
                (Whitespace, 16..17, « »)
                (Int, 17..20, «116»)
                (Comma, 20..21, «,»)
                (CStr, 21..28, «c"\\\\"»)
                (Whitespace, 28..29, « »)
                (Comma, 29..30, «,»)
                (Whitespace, 30..31, « »)
                (Int, 31..34, «234»)
                (Whitespace, 34..35, « »)
                (Comma, 35..36, «,»)
                (Whitespace, 36..37, « »)
                (CStr, 37..44, «c"true"»)
                (Whitespace, 44..45, « »)
                (Comma, 45..46, «,»)
                (Whitespace, 46..47, « »)
                (Ident, 47..52, «false»)
                (Whitespace, 52..53, « »)
                (RSquare, 53..54, «]»)
                (Whitespace, 54..55, « »)
                (Comma, 55..56, «,»)
                (Whitespace, 56..57, « »)
                (CStr, 57..61, «c"t"»)
                (Whitespace, 61..62, « »)
                (Colon, 62..63, «:»)
                (Whitespace, 63..64, « »)
                (CStr, 64..71, «c"\\\""»)
                (Whitespace, 71..72, « »)
                (RCurly, 72..73, «}»)
            "#]],
        );

        check(r#""0123456789012""after""#, &expect![[r#"
            (Str, 0..15, «"0123456789012"»)
            (Str, 15..22, «"after"»)
        "#]]);
        check(r#""01234567890123""after""#, &expect![[r#"
            (Str, 0..16, «"01234567890123"»)
            (Str, 16..23, «"after"»)
        "#]]);
        check(r#""012345678901234""after""#, &expect![[r#"
            (Str, 0..17, «"012345678901234"»)
            (Str, 17..24, «"after"»)
        "#]]);
    }

    #[test]
    fn unterminated_strings() {
        check(r#""unterminated"#, &expect![[r#"
            (Str, 0..13, «"unterminated»)
        "#]]);

        check(r#""unterminated over several chunks"#, &expect![[r#"
            (Str, 0..33, «"unterminated over several chunks»)
        "#]]);
    }

    #[test]
    fn line_comments() {
        check("0123456789abcdef//", &expect![[r"
        (Int, 0..16, «0123456789abcdef»)
        (LineComment, 16..18, «//»)
        "]]);

        check("0123456789abcde//", &expect![[r"
            (Int, 0..15, «0123456789abcde»)
            (LineComment, 15..17, «//»)
        "]]);

        check("0123456789abcdefg//", &expect![[r"
            (Int, 0..17, «0123456789abcdefg»)
            (LineComment, 17..19, «//»)
        "]]);
    }
}
