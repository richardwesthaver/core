//! config/script.rs --- script configuration
use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct ScriptConfig {
  name: String,
  path: Option<String>,
  var: BTreeMap<String, String>,
}
