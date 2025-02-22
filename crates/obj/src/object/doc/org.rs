//! # org
//!
//! Org-mode object types
//!
//! The Org-mode format is particularly difficult to parse, but can be
//! done. The best resources for learning how to create a parser for
//! Org is available in the Org-Element API docs.
//!
//! ## Commentary
//! The `Org` type is the main type used for accessing Org Docs. This
//! object includes builder functions for the `OrgParser` type, which
//! handles parsing of the Org-mode format.
//!
//! This module does not intend to fully support the Org-mode format,
//! and is not a replacement for existing org files.
//!
//! ## Resources
//! - [parse org-mode in elisp](http://ergoemacs.org/emacs/elisp_parse_org_mode.html)
//! - [Using the Mapping API](https://orgmode.org/manual/Using-the-Mapping-API.html)
//! - [Using the Property API](https://orgmode.org/manual/Using-the-Property-API.html)
//! - [Org-element API](https://orgmode.org/worg/dev/org-element-api.html)
//! - [Orgnode.py](http://members.optusnet.com.au/~charles57/GTD/Orgnode.py)
use crate::{object::meta::Property, Objective, Result};
use hash::Id;

use logger::log::info;

use std::{
  collections::HashMap,
  fs,
  path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

/// Org object type
#[derive(Serialize, Deserialize, Debug)]
pub struct Org {
  pub properties: Option<Vec<Property>>,
  pub contents: HashMap<Id, String>,
}

impl Org {
  /// Create a new Org object
  pub fn new() -> Self {
    Org {
      properties: None,
      contents: HashMap::new(),
    }
  }
  /// Create a new Org object from an Org-mode file
  pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
    let content = String::from_utf8(fs::read(PathBuf::from(path.as_ref()))?)
      .expect("failed to read utf8 string");
    let mut org = Org::new();
    org.contents.insert(Id::rand(), content);
    info!("parsed org-file: {}", path.as_ref().display());

    Ok(org)
  }

  /// Append structured data to this Org document
  pub fn append(self, input: String) -> Result<Self> {
    let mut doc = self;
    doc.contents.insert(Id::rand(), input);

    Ok(doc)
  }

  /// Return document content
  pub fn contents(self) -> HashMap<Id, String> {
    self.contents
  }
}

impl Objective for Org {}
