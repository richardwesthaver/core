use super::*;
use zeroize::Zeroizing;
#[test]
fn keyutils_default() {
  default_ks().unwrap();
  join_ss("test_krypt").unwrap();
}
