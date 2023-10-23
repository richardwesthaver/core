//! cfg::config::repo
//!
//! Repo configuration primitives
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
#[cfg(feature = "git")]
pub mod git;
#[cfg(feature = "hg")]
pub mod hg;
/// Generic repo configuration type
///
/// Wraps Mercurial and Git repos
#[derive(Serialize, Deserialize, Debug, Hash, PartialEq)]
pub struct RepoConfig {
  pub vcs: String,
  pub origin: String,
  pub path: PathBuf,
}

impl RepoConfig {
  /// Create a new RepoConfig
  pub fn new() -> Self {
    RepoConfig::default()
  }
}

impl Default for RepoConfig {
  fn default() -> Self {
    RepoConfig {
      vcs: "hg".to_string(),
      origin: "".to_string(),
      path: PathBuf::from("."),
    }
  }
}

/// Subrepo type
///
/// Note that Mercurial subrepos are a 'feature of last resort'
/// according to official docs. They are needed in very niche
/// scenarios and shouldn't be used most of the time.
#[derive(Default)]
pub struct SubRepo {
  pub vcs: String,
  pub origin: String,
  pub path: String,
}
