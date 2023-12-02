use crate::linux_keyutils::{KeyRing, KeyRingIdentifier};
use zeroize::Zeroizing;

#[test]
fn default_user_keyring() {
  let ring =
    KeyRing::from_special_id(KeyRingIdentifier::Session, false).unwrap();
  ring.add_key("krypt_test_default", "hackme").unwrap();
  let key = ring.search("krypt_test_default").unwrap();
  let mut buf = Zeroizing::new([0u8; 2048]);
  let len = key.read(&mut buf).unwrap();
  assert_eq!("hackme", std::str::from_utf8(&buf[..len]).unwrap());
  key.invalidate().unwrap();
}
