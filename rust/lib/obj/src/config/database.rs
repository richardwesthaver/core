//! cfg::config::database
//!
//! Database configuration primitives
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq)]
pub struct DatabaseConfig {
  engine: DatabaseType,
  path: String,
  cfs: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Hash, PartialEq, Eq)]
pub enum DatabaseType {
  RocksDB,
  Postgres,
  Alch,
}
