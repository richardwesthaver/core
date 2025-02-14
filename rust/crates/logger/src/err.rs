//! logger errors
use std::{fmt, io};

use flexi_logger::FlexiLoggerError;

/// logger Result type
pub type Result<T> = std::result::Result<T, Error>;

/// logger Error type
pub enum Error {
  Io(io::Error),
  Flexi(FlexiLoggerError),
  Log(log::SetLoggerError),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Io(ref err) => write!(f, "IO error: {}", err),
      Error::Flexi(ref err) => write!(f, "FlexiLog error: {}", err),
      Error::Log(ref err) => write!(f, "Log error: {}", err),
    }
  }
}

impl fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Io(ref err) => write!(f, "IO error: {}", err),
      Error::Flexi(ref err) => write!(f, "FlexiLog error: {}", err),
      Error::Log(ref err) => write!(f, "Log error: {}", err),
    }
  }
}

impl From<FlexiLoggerError> for Error {
  fn from(err: FlexiLoggerError) -> Self {
    Error::Flexi(err)
  }
}

impl From<log::SetLoggerError> for Error {
  fn from(e: log::SetLoggerError) -> Self {
    Error::Log(e)
  }
}

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Self {
    Error::Io(e)
  }
}
impl std::error::Error for Error {}
