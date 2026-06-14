#![feature(
    custom_inner_attributes,
    explicit_tail_calls,
    loop_match,
    slice_from_ptr_range
)]
#![allow(
    clippy::missing_safety_doc,
    clippy::wildcard_imports,
    incomplete_features,
    unsafe_op_in_unsafe_fn
)]

pub mod common;
pub mod lexers;
pub mod utils;

#[cfg(test)]
mod tests;
