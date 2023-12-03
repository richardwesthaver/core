//// Keyutils
pub use crate::keyutils::{Key, KeyType, Keyring, Permission, SpecialKeyring};
/// Return the default 'special' session for the current thread

pub fn default_ks() -> keyutils::Result<Keyring> {
  Keyring::join_anonymous_session()
}

pub fn join_ks(name: &str) -> keyutils::Result<Keyring> {
  Keyring::join_session(name)
}

pub fn dump_keys(kr: Keyring) -> keyutils::Result<(Vec<Key>, Vec<Keyring>)> {
  kr.read()
}
