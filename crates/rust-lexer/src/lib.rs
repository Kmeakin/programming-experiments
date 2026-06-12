#![feature(
    custom_inner_attributes,
    slice_from_ptr_range,
    loop_match,
    explicit_tail_calls
)]
#![allow(incomplete_features)]

pub mod common;
pub mod lexers;
pub mod utils;

#[cfg(test)]
mod tests;
