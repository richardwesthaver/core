use super::*;

#[test]
fn keyutils_default() {
  ks::default_ks().unwrap();
  ks::join_ks("test_krypt").unwrap();
}
