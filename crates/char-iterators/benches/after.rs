#![allow(clippy::unit_arg)]
#![feature(test)]

extern crate test;

use std::hint::black_box;

use char_iterators::after::Chars;
use char_iterators::corpora;
use test::Bencher;

mod chars_next {
    use super::*;

    fn bench(b: &mut Bencher, corpus: &str) { b.iter(|| black_box(Chars::new(corpus).next())); }

    #[bench]
    fn en(b: &mut Bencher) { bench(b, corpora::en::HUGE); }

    #[bench]
    fn zh(b: &mut Bencher) { bench(b, corpora::zh::HUGE); }

    #[bench]
    fn ru(b: &mut Bencher) { bench(b, corpora::ru::HUGE); }

    #[bench]
    fn emoji(b: &mut Bencher) { bench(b, corpora::emoji::HUGE); }
}

mod chars_rev_next {
    use super::*;

    fn bench(b: &mut Bencher, corpus: &str) {
        b.iter(|| black_box(Chars::new(corpus).next_back()));
    }

    #[bench]
    fn en(b: &mut Bencher) { bench(b, corpora::en::HUGE); }

    #[bench]
    fn zh(b: &mut Bencher) { bench(b, corpora::zh::HUGE); }

    #[bench]
    fn ru(b: &mut Bencher) { bench(b, corpora::ru::HUGE); }

    #[bench]
    fn emoji(b: &mut Bencher) { bench(b, corpora::emoji::HUGE); }
}

mod chars_for_each {
    use super::*;

    fn bench(b: &mut Bencher, corpus: &str) {
        b.iter(|| black_box(Chars::new(corpus).for_each(|_| {})));
    }

    #[bench]
    fn en(b: &mut Bencher) { bench(b, corpora::en::HUGE); }

    #[bench]
    fn zh(b: &mut Bencher) { bench(b, corpora::zh::HUGE); }

    #[bench]
    fn ru(b: &mut Bencher) { bench(b, corpora::ru::HUGE); }

    #[bench]
    fn emoji(b: &mut Bencher) { bench(b, corpora::emoji::HUGE); }
}

mod chars_rev_for_each {
    use super::*;

    fn bench(b: &mut Bencher, corpus: &str) {
        b.iter(|| black_box(Chars::new(corpus).rev().for_each(|_| {})));
    }

    #[bench]
    fn en(b: &mut Bencher) { bench(b, corpora::en::HUGE); }

    #[bench]
    fn zh(b: &mut Bencher) { bench(b, corpora::zh::HUGE); }

    #[bench]
    fn ru(b: &mut Bencher) { bench(b, corpora::ru::HUGE); }

    #[bench]
    fn emoji(b: &mut Bencher) { bench(b, corpora::emoji::HUGE); }
}

mod chars_sum {
    use char_iterators::after::Chars;

    use super::*;

    fn bench(b: &mut Bencher, corpus: &str) {
        b.iter(|| black_box(Chars::new(corpus).map(u32::from).sum::<u32>()));
    }

    #[bench]
    fn en(b: &mut Bencher) { bench(b, corpora::en::HUGE); }

    #[bench]
    fn zh(b: &mut Bencher) { bench(b, corpora::zh::HUGE); }

    #[bench]
    fn ru(b: &mut Bencher) { bench(b, corpora::ru::HUGE); }

    #[bench]
    fn emoji(b: &mut Bencher) { bench(b, corpora::emoji::HUGE); }
}

mod chars_sum_rev {
    use super::*;

    fn bench(b: &mut Bencher, corpus: &str) {
        b.iter(|| black_box(Chars::new(corpus).rev().map(u32::from).sum::<u32>()));
    }

    #[bench]
    fn en(b: &mut Bencher) { bench(b, corpora::en::HUGE); }

    #[bench]
    fn zh(b: &mut Bencher) { bench(b, corpora::zh::HUGE); }

    #[bench]
    fn ru(b: &mut Bencher) { bench(b, corpora::ru::HUGE); }

    #[bench]
    fn emoji(b: &mut Bencher) { bench(b, corpora::emoji::HUGE); }
}
