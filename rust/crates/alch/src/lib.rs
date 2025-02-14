//! alch - memory allocation collection
//!
//! This crate contains a variety of custom allocators optimized for
//! specific use-cases.
#![feature(allocator_api)]
mod tr;
pub use tr::TrAlloc;

pub use bumpalo::Bump;

// https://os.phil-opp.com/allocator-designs/
// mod bump;
// mod block;
// mod slab;
// mod buddy;
