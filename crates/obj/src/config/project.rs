//! Project configurations
use crate::PackageConfig;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct ProjectConfig {
  pub name: String,
  pub packages: Vec<PackageConfig>,
}
