//// Keyutils
pub use crate::keyutils::{KeyType, Keyring, Permission, SpecialKeyring};
/// Return the default 'special' session for the current thread
pub fn default_ks() -> keyutils::Result<Keyring> {
  Keyring::join_anonymous_session()
}

pub fn join_ks(name: &str) -> keyutils::Result<Keyring> {
  Keyring::join_session(name)
}
