//! Configuration for Meta files
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct MetaConfig {
  pub name: String,
  pub src: String,
  pub dst: String,
}
