use crate::registry::Registry;
use tempfile::tempdir;

#[test]
fn open_registry_test() {
  let path = tempdir().unwrap().into_path();
  Registry::new(path).unwrap();
}

#[test]
fn repair_registry_test() {
  let path = tempdir().unwrap();
  Registry::new(path.path()).unwrap();
  Registry::repair(path.path()).unwrap();
}
