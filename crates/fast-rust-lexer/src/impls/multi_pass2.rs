#![allow(clippy::wildcard_imports)]

use std::bstr::ByteStr;

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

        let output_vec = vec![0u32; input_vec.len()];
        (input_vec, output_vec)
    }
}

fn prefix_xor(mut bits: u64) -> u64 {
    bits ^= bits << 1;
    bits ^= bits << 2;
    bits ^= bits << 4;
    bits ^= bits << 8;
    bits ^= bits << 16;
    bits ^= bits << 32;
    bits
}

/// Return a mask with 1s at each character in a `// ... \n` comment body.
///
/// `line_comment_open` marks the second slash of `//`, so comment body starts
/// at `line_comment_open << 1`. This uses a set/reset scan rather than xor
/// toggling, so repeated starts or repeated newlines are handled correctly.
fn line_comment_body_mask<const VEC_LEN: usize>(
    line_comment_open: u64,
    newlines: u64,
    mut prev_in_line_comment: bool,
) -> (u64, bool) {
    let starts = line_comment_open << 1;
    let mut in_comment = u64::from(prev_in_line_comment);
    let mut body = 0;

    for i in 0..VEC_LEN {
        let set = (starts >> i) & 1;
        let reset = (newlines >> i) & 1;
        in_comment = (in_comment | set) & (reset ^ 1);
        body |= in_comment << i;
    }

    prev_in_line_comment = in_comment != 0;
    (body, prev_in_line_comment)
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
    let next_is_escaped = prev_escaped;
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
    let potential_escape = backslashes & !u64::from(next_is_escaped);

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
    let escaped = escape_and_terminal_code ^ (backslashes | u64::from(next_is_escaped));
    prev_escaped = (escape_and_terminal_code & backslashes) >> (VEC_LEN - 1) != 0;
    (escaped, prev_escaped)
}

fn idents_mask<const VEC_LEN: usize>(ident_chars: u64, mut prev_ident: bool) -> (u64, bool) {
    // 1 at the start of each ident
    // eg
    // foo bar foobar
    // 10001000100000
    let id_starts = ident_chars & !(ident_chars << 1) & !u64::from(prev_ident);

    // 1 at the end of each ident
    // eg
    // foo bar foobar
    // 00100010000001
    let id_ends = ident_chars & !(ident_chars >> 1);

    // 1 at the start and end of each ident
    // eg
    // foo bar foobar
    // 10101010100001
    let idents = id_starts | (id_ends << 1);

    prev_ident = ident_chars >> (VEC_LEN - 1) != 0;
    (idents, prev_ident)
}

fn whitespace_mask<const VEC_LEN: usize>(
    whitespace_chars: u64,
    mut prev_whitespace: bool,
) -> (u64, bool) {
    // 1 at the start of each whitespace run
    // eg
    // foo bar foobar
    // 10001000100000
    let whitespace_starts =
        whitespace_chars & !(whitespace_chars << 1) & !u64::from(prev_whitespace);

    // 1 at the end of each whitespace run
    // eg
    // foo bar foobar
    // 00100010000001
    let whitespace_ends = whitespace_chars & !(whitespace_chars >> 1);

    // 1 at the start and end of each whitespace run
    // eg
    // foo bar foobar
    // 10101010100001
    let whitespaces = whitespace_starts | (whitespace_ends << 1);

    prev_whitespace = whitespace_chars >> (VEC_LEN - 1) != 0;
    (whitespaces, prev_whitespace)
}

/// Like `eprintln`, but only when `debug_assertions` are enabled.
macro_rules! deprintln {
    ($($args:tt)*) => {
        if cfg!(debug_assertions) {
            eprintln!($($args)*);
        }
    };
}

pub fn stage1<'a, const VEC_LEN: usize>(src: &[u8], out_slice: &'a mut [u32]) -> &'a mut [u32] {
    #[allow(non_snake_case)]
    let ALL_ONES: u64 = const {
        match VEC_LEN {
            16 => 0xFFFF,
            32 => 0xFFFF_FFFF,
            64 => 0xFFFF_FFFF_FFFF_FFFF,
            _ => unreachable!(),
        }
    };

    unsafe {
        debug_assert!(out_slice.len() >= src.len());
        debug_assert_eq!(src.last_chunk(), Some(&[EOF_BYTE; VEC_LEN]));
        debug_assert_eq!(
            src.rchunks_exact(VEC_LEN).nth(1),
            Some([EOF_BYTE; VEC_LEN].as_slice())
        );

        let src_start = src.as_ptr();
        let src_end = src.as_ptr_range().end.sub(VEC_LEN);
        let mut src_ptr = src_start;
        let mut out_ptr = out_slice.as_mut_ptr();

        let mut idx = 0;
        let mut prev_whitespace = false;
        let mut prev_ident = false;
        let mut prev_string = false;
        let mut prev_escaped = false;
        let mut prev_slash = false;
        let mut prev_in_line_comment = false;
        loop {
            let vec = load::<VEC_LEN>(src_ptr);
            let eofs = movemask(eq(vec, EOF_BYTE));
            let ws_chars =
                movemask(eq(vec, b' ') | eq(vec, b'\t') | eq(vec, b'\n') | eq(vec, b'\r'));
            let quotes = movemask(eq(vec, b'"'));
            let backslashes = movemask(eq(vec, b'\\'));
            let slashes = movemask(eq(vec, b'/'));
            let newlines = movemask(eq(vec, b'\n'));
            let ident_chars = movemask(
                eq(vec, b'_')
                    | in_range(vec, b'a', b'z')
                    | in_range(vec, b'A', b'Z')
                    | in_range(vec, b'0', b'9'),
            );
            deprintln!("vec           = {}", ByteStr::new(vec.as_array()));
            deprintln!("eof           = {}", fmt_bitmask::<VEC_LEN>(eofs));
            deprintln!("ws_chars      = {}", fmt_bitmask::<VEC_LEN>(ws_chars));
            deprintln!("quotes        = {}", fmt_bitmask::<VEC_LEN>(quotes));
            deprintln!("backslashes   = {}", fmt_bitmask::<VEC_LEN>(backslashes));
            deprintln!("ident_chars   = {}", fmt_bitmask::<VEC_LEN>(ident_chars));
            deprintln!("slashes       = {}", fmt_bitmask::<VEC_LEN>(slashes));
            deprintln!("newlines      = {}", fmt_bitmask::<VEC_LEN>(newlines));

            let escaped_chars;
            (escaped_chars, prev_escaped) =
                escaped_chars_mask::<VEC_LEN>(backslashes, prev_escaped);
            let real_quotes = quotes & !escaped_chars;
            deprintln!("escaped_chars = {}", fmt_bitmask::<VEC_LEN>(escaped_chars));
            deprintln!("real_quotes   = {}", fmt_bitmask::<VEC_LEN>(real_quotes));

            let strings = prefix_xor(real_quotes) ^ (if prev_string { ALL_ONES } else { 0 });
            prev_string = strings >> (VEC_LEN - 1) != 0;
            deprintln!("strings       = {}", fmt_bitmask::<VEC_LEN>(strings));
            let open_quotes = real_quotes & strings;
            let close_quotes = real_quotes & !strings;
            deprintln!("open_quotes   = {}", fmt_bitmask::<VEC_LEN>(open_quotes));
            deprintln!("close_quotes  = {}", fmt_bitmask::<VEC_LEN>(close_quotes));

            let slash2 = slashes & (slashes << 1 | u64::from(prev_slash));
            prev_slash = slashes >> (VEC_LEN - 1) != 0;
            deprintln!("slash2        = {}", fmt_bitmask::<VEC_LEN>(slash2));

            let line_comment_open = slash2 & !strings;
            let line_comment_ranges;
            (line_comment_ranges, prev_in_line_comment) = line_comment_body_mask::<VEC_LEN>(
                line_comment_open,
                newlines,
                prev_in_line_comment,
            );
            deprintln!(
                "line_comment  = {}",
                fmt_bitmask::<VEC_LEN>(line_comment_ranges)
            );

            let whitespace;
            (whitespace, prev_whitespace) = whitespace_mask::<VEC_LEN>(ws_chars, prev_whitespace);
            deprintln!("whitespace    = {}", fmt_bitmask::<VEC_LEN>(whitespace));

            let idents;
            (idents, prev_ident) = idents_mask::<VEC_LEN>(ident_chars, prev_ident);
            deprintln!("idents        = {}", fmt_bitmask::<VEC_LEN>(idents));

            // Any character that is not an alphanumeric char or a whitespace char, and not
            // in a string, is a punctuation char.
            let puncts = !ws_chars & !ident_chars & !strings & !close_quotes & !eofs;
            deprintln!("punct         = {}", fmt_bitmask::<VEC_LEN>(puncts));

            let mask = ((idents | puncts | whitespace) & !strings) | open_quotes;
            let mask = mask & !close_quotes & !line_comment_ranges;

            let mut mask = (mask | eofs) & ALL_ONES;
            deprintln!("mask          = {}", fmt_bitmask::<VEC_LEN>(mask));
            deprintln!("vec           = {}", ByteStr::new(vec.as_array()));

            let next_out_ptr = out_ptr.add(mask.count_ones() as usize);
            while mask != 0 {
                out_ptr = write_and_advance(out_ptr, idx + mask.trailing_zeros());
                mask = mask & (mask - 1);
            }
            out_ptr = next_out_ptr;

            src_ptr = src_ptr.add(VEC_LEN);
            idx += VEC_LEN as u32;
            if src_ptr >= src_end {
                deprintln!("done\n");
                break;
            }
            deprintln!();
        }
        let out_len = out_ptr.offset_from_unsigned(out_slice.as_mut_ptr());
        &mut out_slice[..out_len]
    }
}

pub fn stage2<const VEC_LEN: usize>(src: &[u8], indexes: &[u32]) -> Vec<(TokenKind, u32, u32)> {
    let mut tokens = Vec::new();
    let mut indexes = indexes.iter().copied();
    let Some(mut start) = indexes.next() else {
        return tokens;
    };
    loop {
        match src[start as usize] {
            b'/' if src[start as usize + 1] == b'/' => {
                let slash2 = indexes.next().unwrap();
                debug_assert_eq!(slash2, start + 1);

                let newline = indexes.next().unwrap();
                debug_assert!(src[newline as usize] == b'\n' || src[newline as usize] == EOF_BYTE);
                tokens.push((TokenKind::LineComment, start, newline));
                start = newline;
            }

            b @ (b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b';' | b':' | b'+' | b'-'
            | b'*' | b'%' | b'=' | b'&' | b'|' | b'$' | b'?' | b'~' | b'#' | b'@' | b'.'
            | b'!' | b'>' | b'<' | b'^' | b'/') => {
                let end = indexes.next().unwrap();
                let kind = TokenKind::from_u8(b).unwrap();

                debug_assert_eq!(end, start + 1, "start = {start}, end = {end}");
                tokens.push((kind, start, end));
                start = end;
            }
            b' ' | b'\t' | b'\n' | b'\r' => {
                let end = indexes.next().unwrap();
                tokens.push((TokenKind::Whitespace, start, end));
                start = end;
            }
            b'"' => {
                let exclusive_end = indexes.next().unwrap();
                tokens.push((TokenKind::Str, start, exclusive_end));
                start = exclusive_end;
            }
            b'b' if src[start as usize + 1] == b'"' => {
                let open_quote = indexes.next().unwrap();
                debug_assert_eq!(
                    open_quote,
                    start + 1,
                    "start = {start}, open_quote = {open_quote}"
                );
                let exclusive_end = indexes.next().unwrap();
                tokens.push((TokenKind::ByteStr, start, exclusive_end));
                start = exclusive_end;
            }
            b'c' if src[start as usize + 1] == b'"' => {
                let open_quote = indexes.next().unwrap();
                debug_assert_eq!(
                    open_quote,
                    start + 1,
                    "start = {start}, open_quote = {open_quote}"
                );
                let exclusive_end = indexes.next().unwrap();
                tokens.push((TokenKind::CStr, start, exclusive_end));
                start = exclusive_end;
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let end = indexes.next().unwrap();
                tokens.push((TokenKind::Ident, start, end));
                start = end;
            }
            b'0'..=b'9' => {
                let end = indexes.next().unwrap();
                tokens.push((TokenKind::Int, start, end));
                start = end;
            }

            b'\'' => todo!(),
            EOF_BYTE => break,
            b => todo!("unhandled byte: 0x{b:02x}"),
        }
    }
    tokens
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
            (16, «�»)
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
        check("  hello  world  ", &expect![[r"
            (0, « »)
            (2, «h»)
            (7, « »)
            (9, «w»)
            (14, « »)
            (16, «�»)
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

        check("  hello+world  ", &expect![[r"
            (0, « »)
            (2, «h»)
            (7, «+»)
            (8, «w»)
            (13, « »)
            (15, «�»)
            (16, «�»)
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
            (32, «�»)
            (33, «�»)
            (34, «�»)
            (35, «�»)
            (36, «�»)
            (37, «�»)
            (38, «�»)
            (39, «�»)
            (40, «�»)
            (41, «�»)
            (42, «�»)
            (43, «�»)
            (44, «�»)
            (45, «�»)
            (46, «�»)
            (47, «�»)
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
            (32, «�»)
            (33, «�»)
            (34, «�»)
            (35, «�»)
            (36, «�»)
            (37, «�»)
            (38, «�»)
            (39, «�»)
            (40, «�»)
            (41, «�»)
            (42, «�»)
            (43, «�»)
            (44, «�»)
            (45, «�»)
            (46, «�»)
            (47, «�»)
        "]]);
        check("!#///*\n", &expect![[r"
            (0, «!»)
            (1, «#»)
            (2, «/»)
            (3, «/»)
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
            (16, «�»)
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
            (16, «�»)
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
            (16, «�»)
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
        "#]]);

        check(r#"""#, &expect![[r#"
            (0, «"»)
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
            (16, «�»)
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
        "#]]);

        check(r#""123456789abcdef"#, &expect![[r#"
            (0, «"»)
            (16, «�»)
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
        "#]]);
        check(r#""123456789abcdef0"#, &expect![[r#"
            (0, «"»)
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
            (32, «�»)
            (33, «�»)
            (34, «�»)
            (35, «�»)
            (36, «�»)
            (37, «�»)
            (38, «�»)
            (39, «�»)
            (40, «�»)
            (41, «�»)
            (42, «�»)
            (43, «�»)
            (44, «�»)
            (45, «�»)
            (46, «�»)
            (47, «�»)
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
                (44, «�»)
                (45, «�»)
                (46, «�»)
                (47, «�»)
                (48, «�»)
                (49, «�»)
                (50, «�»)
                (51, «�»)
                (52, «�»)
                (53, «�»)
                (54, «�»)
                (55, «�»)
                (56, «�»)
                (57, «�»)
                (58, «�»)
                (59, «�»)
                (60, «�»)
                (61, «�»)
                (62, «�»)
                (63, «�»)
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

    #[track_caller]
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
            (Slash, 2..3, «/»)
            (Star, 3..4, «*»)
            (Whitespace, 4..5, «
            »)
            (Star, 5..6, «*»)
            (Slash, 6..7, «/»)
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
        check("//", &expect![[r"
        (LineComment, 0..2, «//»)
        "]]);

        check("//\n", &expect![[r"
            (LineComment, 0..2, «//»)
            (Whitespace, 2..3, «
            »)
        "]]);

        check("//foo\n", &expect![[r"
            (LineComment, 0..5, «//foo»)
            (Whitespace, 5..6, «
            »)
        "]]);

        check("//foo //bar\n", &expect![[r"
            (LineComment, 0..11, «//foo //bar»)
            (Whitespace, 11..12, «
            »)
        "]]);

        check("//foo\n//bar\n", &expect![[r"
            (LineComment, 0..5, «//foo»)
            (Whitespace, 5..6, «
            »)
            (LineComment, 6..11, «//bar»)
            (Whitespace, 11..12, «
            »)
        "]]);

        check("//foo\nbar\n//foobar", &expect![[r"
            (LineComment, 0..5, «//foo»)
            (Whitespace, 5..6, «
            »)
            (Ident, 6..9, «bar»)
            (Whitespace, 9..10, «
            »)
            (LineComment, 10..18, «//foobar»)
        "]]);
    }
}
