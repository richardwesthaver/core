//! cfg::config::library
//!
//! Library configuration primitives
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Software library configuration
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct LibraryConfig {
  name: String,
  scripts: HashMap<String, String>,
}
