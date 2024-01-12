//! db modules
//!
//! This library provides types and builder functions for working with
//! databases. Currently the only backend supported is RocksDB.
pub mod backup;
pub mod blob;
pub mod cache;
pub mod comp;
mod err;
#[cfg(feature="rocksdb")]
pub mod registry;

pub use err::{Error, Result};

#[cfg(feature="rocksdb")]
pub use rocksdb::{ColumnFamilyDescriptor, DBWithThreadMode, MultiThreaded, Options, DB};

use std::{path::PathBuf, sync::Arc};

#[cfg(test)]
mod tests;

#[cfg(feature="rocksdb")]
/// RocksDB handle
pub struct RocksDB {
  pub path: PathBuf,
  pub db: Arc<DB>,
}

#[cfg(feature="rocksdb")]
impl RocksDB {
  pub fn new() -> Self {
    let path = PathBuf::from(".rdb");
    let db = DB::open_default(&path).unwrap();
    let db = Arc::new(db);
    RocksDB { path: path, db: db }
  }
}
