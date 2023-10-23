//! # Meta types
use crate::Objective;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
/// Meta object
///
/// This struct is built into other Objects and provides contextual data
#[derive(Serialize, Deserialize, Debug, Hash)]
pub struct Meta {
  tags: Option<String>,
  properties: Option<Vec<Property>>,
  timestamp: DateTime<Utc>,
}

impl Meta {
  pub fn new() -> Self {
    Meta {
      tags: None,
      properties: None,
      timestamp: Utc::now(),
    }
  }
}

impl Default for Meta {
  fn default() -> Self {
    Meta::new()
  }
}

impl Objective for Meta {}

/// Property object
///
/// An isolated property consisting of a (key,val) pair.
///
/// TODO <2021-08-17 Tue 01:28> - this should be a trait. make
/// metadata module for this.
#[derive(Serialize, Deserialize, Debug, Hash)]
pub struct Property {
  key: String,
  val: String,
}

/// Summary object
///
/// A summary of an object.
///
/// TODO <2021-08-17 Tue 01:30> make this a trait too.
#[derive(Serialize, Deserialize, Debug, Hash)]
pub struct Summary {
  meta: Meta,
  summary: String,
}

/// Note object
///
/// A note pertaining to one or many objects.
#[derive(Serialize, Deserialize, Debug, Hash)]
pub struct Note {
  meta: Meta,
  content: String,
}
