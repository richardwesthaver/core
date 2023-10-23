//! cfg::config::display
//!
//! The structs in this module pertain to PHYSICAL Display
//! configurations. They immplement functionality intended for use in
//! OS system and user configurations. For example: xrandr
//! configuration.
use serde::{Deserialize, Serialize};

/// Display configuration
#[derive(Serialize, Deserialize, Debug, Hash)]
pub struct DisplayConfig {
  pub name: String,
  pub resolution: (u16, u16),
  pub output: String,
  pub primary: bool,
  pub pos: String,
  pub rotate: String,
}

impl Default for DisplayConfig {
  fn default() -> Self {
    DisplayConfig {
      name: "".to_string(),
      output: "".to_string(),
      resolution: (1920, 1080),
      primary: true,
      pos: "0x0".to_string(),
      rotate: "normal".to_string(),
    }
  }
}
