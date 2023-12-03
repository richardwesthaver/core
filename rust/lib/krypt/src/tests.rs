use super::*;
use zeroize::Zeroizing;
#[test]
fn keyutils_default() {
  ks::default_ks().unwrap();
  ks::join_ks("test_krypt").unwrap();
}
