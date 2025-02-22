//! Color object
//!
//! Generic color definition structured as RGBA8
//!
//! TODO <2021-08-16 Mon 21:45> - repr in RGBA8,16,32
use crate::Objective;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Hash, Default)]
pub struct Color {
  red: u8,
  green: u8,
  blue: u8,
  alpha: u8,
}

impl Objective for Color {}
