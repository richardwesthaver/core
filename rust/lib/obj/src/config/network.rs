//! cfg::config::network
//!
//! Network configuration primitives
use serde::{Deserialize, Serialize};
use std::{fmt, net::SocketAddr};

/// Network configuration
#[derive(Serialize, Deserialize, Hash, Debug, PartialEq, Clone)]
pub struct NetworkConfig {
  /// a socket to bind
  pub socket: SocketAddr,
  /// a proxy to forward packets from
  pub proxy: Option<SocketAddr>,
  /// tunnel to use
  pub tunnel: Option<String>,
  /// network engine to attach
  pub engine: EngineType,
  /// peers to register AOT
  pub peers: Option<Vec<SocketAddr>>,
}

impl Default for NetworkConfig {
  fn default() -> Self {
    NetworkConfig {
      socket: "127.0.0.1:0".parse().unwrap(),
      proxy: None,
      tunnel: None,
      engine: EngineType::default(),
      peers: None,
    }
  }
}

#[derive(Serialize, Deserialize, Hash, Debug, PartialEq, Clone)]
pub enum EngineType {
  Quic,
  Http,
  Dns,
  Ssh,
  Uds,
}

impl Default for EngineType {
  fn default() -> Self {
    Self::Http
  }
}

impl std::fmt::Display for EngineType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      EngineType::Quic => write!(f, "quic"),
      EngineType::Http => write!(f, "http"),
      EngineType::Dns => write!(f, "dns"),
      EngineType::Ssh => write!(f, "ssh"),
      EngineType::Uds => write!(f, "uds"),
    }
  }
}
