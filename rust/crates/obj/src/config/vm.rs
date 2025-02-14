//! config/vm.rs --- Virtual Machine configuration
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct VmConfig {
  pub name: String,
}
