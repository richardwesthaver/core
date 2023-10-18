//! hash - wrapper for hash algorithms and types

pub use blake3::{derive_key, hash, keyed_hash, Hash as B3Hash, Hasher as B3Hasher, OutputReader};
pub use hex;
use rand::Rng;
use serde::{Deserialize, Serialize};
pub use sha2::Sha512;

pub use std::hash::{Hash, Hasher};

pub const KEY_LEN: usize = 32;
pub const OUT_LEN: usize = 32;
pub const OUT_LEN_HEX: usize = OUT_LEN * 2;

//mod tree;

/// a simple Id abstraction with help functions. I'm finding this easier than
/// state machines and traits for the time-being.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize, Hash)]
pub struct Id(pub Vec<u8>);

impl Id {
  pub fn rand() -> Self {
    let mut rng = rand::thread_rng();
    let vals: Vec<u8> = (0..KEY_LEN).map(|_| rng.gen_range(0..u8::MAX)).collect();
    Id(vals)
  }

  pub fn state_hash(&self, state: &mut B3Hasher) -> Self {
    let mut output = vec![0; OUT_LEN];
    state.update(&self.0);
    let mut res = state.finalize_xof();
    res.fill(&mut output);
    Id(output)
  }

  pub fn to_hex(&self) -> String {
    hex::encode(&self.0)
  }
}

/// PeerId
///
/// identifies a unique Peer
#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd, Debug)]
pub struct PeerId {
  id: [u8; 32],
}

impl PeerId {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn rand() -> Self {
    let pd = rand::thread_rng().gen::<[u8; 32]>();
    Self { id: pd }
  }

  pub fn from_bytes(data: &[u8]) -> Self {
    let pd = blake3::hash(data);
    let hash = pd.as_bytes();
    Self { id: *hash }
  }
}

impl Default for PeerId {
  fn default() -> Self {
    PeerId { id: [0; 32] }
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn id_state_hash() {
    let id = crate::Id(vec![0; crate::KEY_LEN]);
    let hash = id.state_hash(&mut crate::B3Hasher::new());
    assert_eq!(hash, id.state_hash(&mut crate::B3Hasher::new()));
  }

  #[test]
  fn id_hex() {
    let id = crate::Id(vec![255; crate::KEY_LEN]);

    assert_eq!(
      hex::decode("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").unwrap(),
      id.0
    );
  }

  #[test]
  fn rand_id() {
    let id = crate::Id::rand();
    let hash = id.state_hash(&mut crate::B3Hasher::new());
    assert_eq!(hash, id.state_hash(&mut crate::B3Hasher::new()));
  }

  #[test]
  fn random_demon_id_is_valid() {
    use crate::PeerId;
    for _ in 0..5000 {
      let did = PeerId::rand();
      let did2 = PeerId::rand();
      assert_eq!(did, did);
      assert_ne!(did, did2);
    }
  }
}
