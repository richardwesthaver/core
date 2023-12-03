//! lib.rs --- Krypt Key Management Library
pub mod ks;
pub mod ss;

pub use keyutils;

use obj::{AuthConfig, Configure, DatabaseConfig, Objective, Result};
use serde::{Deserialize, Serialize};
use std::{fs, path::PathBuf};

#[cfg(test)]
mod tests;

#[derive(Serialize, Deserialize, Hash, Debug, Clone, Default)]
pub struct KryptConfig {
  auth: AuthConfig,
  db: DatabaseConfig,
}

impl KryptConfig {
  pub fn load_file(path: PathBuf) -> Result<KryptConfig> {
    match fs::read_to_string(&path) {
      Ok(cfg) => KryptConfig::from_json_str(&cfg),
      Err(e) => Err(e.into()),
    }
  }
}

impl Objective for KryptConfig {}
impl Configure for KryptConfig {}
