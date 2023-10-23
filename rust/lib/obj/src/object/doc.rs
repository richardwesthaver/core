//! obj::doc
//!
//! Document object types
#[cfg(feature = "org")]
pub mod org;

use crate::Objective;
use serde::{Deserialize, Serialize};
use std::{fmt, str::FromStr};

/// Document object
#[derive(Serialize, Deserialize, Debug, Hash, PartialEq)]
pub struct Doc {
  pub extension: DocExtension,
}

impl Doc {
  pub fn new(ext: &str) -> Self {
    Doc {
      extension: DocExtension::from_str(ext).unwrap(),
    }
  }
}

impl Default for Doc {
  fn default() -> Self {
    Doc::new("org")
  }
}

impl Objective for Doc {}

/// Document extensions. Use in filenames and IO matching in some
/// cases
#[derive(Serialize, Deserialize, Debug, Hash, PartialEq)]
pub enum DocExtension {
  OrgExt,
  PdfExt,
  HtmlExt,
  None,
}

impl fmt::Display for DocExtension {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      DocExtension::OrgExt => write!(f, "org"),
      DocExtension::PdfExt => write!(f, "pdf"),
      DocExtension::HtmlExt => write!(f, "html"),
      DocExtension::None => write!(f, ""),
    }
  }
}

impl FromStr for DocExtension {
  type Err = ();
  fn from_str(input: &str) -> Result<DocExtension, Self::Err> {
    match input {
      "org" => Ok(DocExtension::OrgExt),
      "pdf" => Ok(DocExtension::PdfExt),
      "html" => Ok(DocExtension::HtmlExt),
      "" => Ok(DocExtension::None),
      _ => Err(()),
    }
  }
}
