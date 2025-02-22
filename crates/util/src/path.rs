//! path module
//!
//! Helper functions for working with paths on filesystem
use std::{
  io::Error,
  path::{Component, Path, PathBuf},
};

/// Given a path provided by the user, determines where generated files
/// related to that path should go.
// this may be for flate only- move there
pub fn local_relative_path<P: AsRef<Path>>(path: P) -> Result<PathBuf, Error> {
  let mut rel_path = PathBuf::new();
  let path = path.as_ref();
  for component in path.components() {
    match component {
      Component::Prefix(_) | Component::RootDir | Component::CurDir => {}
      Component::ParentDir => drop(rel_path.pop()), // noop if empty
      Component::Normal(name) => rel_path.push(name),
    }
  }

  Ok(rel_path)
}
