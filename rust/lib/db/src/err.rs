//! db errors
use std::{fmt, io};

/// db Result type
pub type Result<T> = std::result::Result<T, Error>;

/// db Error type
pub enum Error {
  Io(io::Error),
  Rdb(rocksdb::Error),
}

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Self {
    Error::Io(e)
  }
}

impl From<rocksdb::Error> for Error {
  fn from(e: rocksdb::Error) -> Self {
    Error::Rdb(e)
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Io(ref err) => write!(f, "IO error: {}", err),
      Error::Rdb(ref err) => write!(f, "RocksDB error: {}", err),
    }
  }
}

impl fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Io(ref err) => write!(f, "IO error: {}", err),
      Error::Rdb(ref err) => write!(f, "RocksDB error: {}", err),
    }
  }
}

impl std::error::Error for Error {}
