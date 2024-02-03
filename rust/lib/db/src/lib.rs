//! db modules
//!
//! This library provides types and builder functions for working with
//! databases. Currently the only backend supported is RocksDB.
#![feature(associated_type_defaults)]
use std::path::PathBuf;

#[cfg(feature = "rocksdb")]
pub use rocksdb;

mod err;
pub use err::{Error, Result};

#[cfg(test)]
mod tests;

pub trait Db {
  #[cfg(feature = "rocksdb")]
  type DB = rocksdb::DB;
  #[cfg(not(feature = "rocksdb"))]
  type DB;
  fn db_init(&self) -> Result<()>;
  fn db_open(&self) -> Result<()>;
  fn db_close(&self) -> Result<()>;
  fn db_query(&self) -> Result<()>;
  fn db_transaction(&self) -> Result<()>;
  fn db_set_config<C:DbConfigExt>(&self, cfg: C) -> Self;
  fn db_get_config<C:DbConfigExt>(&self) -> C;
}

pub trait DbConfigExt {
  fn db_path(&self) -> Option<PathBuf>;
  fn db_user(&self) -> Option<String>;
  fn set_db_config_value<T>(&self, key: &str, val: T) -> Self;
  fn get_db_config_value<T>(&self, key: &str) -> T;
}
