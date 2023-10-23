//! net errors
use std::{fmt, io};

/// net Result type
pub type Result<T> = std::result::Result<T, Error>;

/// net Error type
pub enum Error {
  Io(io::Error),
  #[cfg(feature = "client")]
  Reqwest(reqwest::Error),
  #[cfg(feature = "server")]
  Axum(axum::Error),
  #[cfg(feature = "dns")]
  Dns(trust_dns_resolver::error::ResolveError),
  #[cfg(feature = "ssh")]
  Ssh(thrussh::Error),
  Json(serde_json::Error),
  UnexpectedEnd,
}

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Self {
    Error::Io(e)
  }
}

#[cfg(feature = "client")]
impl From<reqwest::Error> for Error {
  fn from(e: reqwest::Error) -> Self {
    Error::Reqwest(e)
  }
}

#[cfg(feature = "server")]
impl From<axum::Error> for Error {
  fn from(e: axum::Error) -> Self {
    Error::Axum(e)
  }
}

#[cfg(feature = "dns")]
impl From<trust_dns_resolver::error::ResolveError> for Error {
  fn from(e: trust_dns_resolver::error::ResolveError) -> Self {
    Error::Dns(e)
  }
}

#[cfg(feature = "ssh")]
impl From<thrussh::Error> for Error {
  fn from(e: thrussh::Error) -> Self {
    Error::Ssh(e)
  }
}

impl From<serde_json::Error> for Error {
  fn from(e: serde_json::Error) -> Self {
    Error::Json(e)
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Io(ref err) => write!(f, "net IO error: {}", err),
      #[cfg(feature = "client")]
      Error::Reqwest(ref err) => write!(f, "net::client Reqwest error: {}", err),
      #[cfg(feature = "server")]
      Error::Axum(ref err) => write!(f, "net::server Axum error: {}", err),
      #[cfg(feature = "dns")]
      Error::Dns(ref err) => write!(f, "net::engine::dns error: {}", err),
      #[cfg(feature = "ssh")]
      Error::Ssh(ref err) => write!(f, "net::engine::ssh error: {}", err),
      Error::Json(ref err) => write!(f, "net Json error: {}", err),
      Error::UnexpectedEnd => write!(f, "unexpected end of buffer."),
    }
  }
}

impl fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Io(ref err) => write!(f, "net IO error: {}", err),
      #[cfg(feature = "client")]
      Error::Reqwest(ref err) => write!(f, "net::client Reqwest error: {}", err),
      #[cfg(feature = "server")]
      Error::Axum(ref err) => write!(f, "net::server Axum error: {}", err),
      #[cfg(feature = "dns")]
      Error::Dns(ref err) => write!(f, "net::server Dns error: {}", err),
      #[cfg(feature = "ssh")]
      Error::Ssh(ref err) => write!(f, "net::engine::ssh error: {}", err),
      Error::Json(ref err) => write!(f, "net Json error: {}", err),
      Error::UnexpectedEnd => write!(f, "unexpected end of buffer."),
    }
  }
}

impl std::error::Error for Error {}
