//! obj::object
//!
//! Concrete object types and traits. All type definitions conform to
//! the Serde spec.
pub mod color;
pub mod contact;
pub mod direction;
pub mod doc;
pub mod location;
pub mod media;
pub mod meta;
pub mod temperature;

#[cfg(test)]
mod test {
  use super::*;
  use crate::Objective;
  use std::{fs, str::FromStr};
  #[test]
  fn test_location_points() {
    let pnt = location::Point::new(1.0, 2.0);
    assert_eq!(
      String::from_str("(\n  lat: 1,\n  lng: 2,\n)").unwrap(),
      pnt.to_ron_string().unwrap()
    );
    assert_eq!(
      location::Point::from_ron_str("(lat: 1.0, lng: 2.0)").unwrap(),
      pnt
    );
  }

  /// test file metadata
  #[test]
  fn test_basic_file_metadata() {
    let attr = fs::metadata("Cargo.toml").unwrap();
    println!("{:?}", attr);
  }

  #[test]
  fn test_docs() {
    let doc = doc::Doc::default();
    assert_eq!(doc.extension, doc::DocExtension::from_str("org").unwrap());
  }

  #[test]
  fn test_media() {
    let media = media::Media::new("test_media", "wav");
    assert_eq!(
      media.extension,
      media::MediaExtension::from_str("wav").unwrap()
    );
  }
}
