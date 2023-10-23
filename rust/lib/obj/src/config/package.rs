//! cfg::config::package
//!
//! Package configuration primitives
use crate::{LibraryConfig, MetaConfig, ProgramConfig, RepoConfig};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single package configuration.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct PackageConfig {
  /// Name of this package
  pub name: String,
  pub repo: RepoConfig,
  pub bin: Option<ProgramConfig>,
  pub lib: Option<LibraryConfig>,
  pub babel: Option<HashMap<String, Vec<String>>>,
  pub meta: Option<MetaConfig>, // will be MetaConfig
}

impl PackageConfig {
  /// Create a new PackageConfig with a given name
  pub fn new(name: &str) -> Self {
    PackageConfig {
      name: name.to_string(),
      repo: RepoConfig::new(),
      bin: None,
      lib: None,
      babel: None,
      meta: None,
    }
  }
}
impl Default for PackageConfig {
  fn default() -> Self {
    PackageConfig {
      name: ".".to_string(),
      repo: RepoConfig::default(),
      bin: None,
      lib: None,
      babel: None,
      meta: None,
    }
  }
}
