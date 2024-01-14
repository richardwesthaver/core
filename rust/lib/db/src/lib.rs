//! db modules
//!
//! This library provides types and builder functions for working with
//! databases. Currently the only backend supported is RocksDB.
#[cfg(feature="rocksdb")]
pub use rocksdb;

mod err;
pub use err::{Error, Result};

#[cfg(test)]
mod tests;
