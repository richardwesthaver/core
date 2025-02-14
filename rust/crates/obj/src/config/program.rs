//! cfg::config::program
//!
//! Program configuration primitives
use crate::RepoConfig;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Program configuration type
#[derive(Serialize, Deserialize, Debug, Default, PartialEq)]
pub struct ProgramConfig {
  pub name: String,
  pub path: String,
  pub repo: Option<RepoConfig>,
  pub build: Option<String>,
  pub scripts: HashMap<String, String>,
}
