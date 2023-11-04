//! hash - wrapper for hash algorithms and types

pub use blake3::{derive_key, hash, keyed_hash, Hash as B3Hash, Hasher as B3Hasher, OutputReader};
pub use hex;
use rand::Rng;
use serde::{Deserialize, Serialize};
pub use sha2::Sha512;
pub use phf;
// hashbrown is now the default for the Rust stdlib. We only need to
// re-export in no_std envs.
#[cfg(no_std)] 
pub use hashbrown::{HashMap, HashSet};
pub use std::hash::{Hash, Hasher};

pub const KEY_LEN: usize = 32;
pub const OUT_LEN: usize = 32;
pub const OUT_LEN_HEX: usize = OUT_LEN * 2;

//mod tree;
#[cfg(test)]
mod tests;
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
