use crate::*;
use hex::decode;
use std::convert::TryInto;

#[test]
fn hex_hash() -> Result<(), Box<dyn std::error::Error>> {
  let mut hasher1 = B3Hasher::new();
  hasher1.update(b"foo");
  hasher1.update(b"bar");
  hasher1.update(b"baz");
  let out1 = hasher1.finalize();
  let mut xof1 = [0; 301];
  hasher1.finalize_xof().fill(&mut xof1);
  assert_eq!(out1.as_bytes(), &xof1[..32]);

  let hash_hex =
    "d74981efa70a0c880b8d8c1985d075dbcbf679b99a5f9914e5aaf96b831a9e24";
  let hash_bytes = decode(hash_hex).unwrap();
  let hash_array: [u8; OUT_LEN] = hash_bytes[..].try_into().unwrap();
  let _: B3Hash = hash_array.into();
  Ok(())
}

#[test]
fn id_state_hash() {
  let id = Id(vec![0; crate::KEY_LEN]);
  let hash = id.state_hash(&mut crate::B3Hasher::new());
  assert_eq!(hash, id.state_hash(&mut crate::B3Hasher::new()));
}

#[test]
fn id_hex() {
  let id = crate::Id(vec![255; crate::KEY_LEN]);

  assert_eq!(
    hex::decode(
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    )
    .unwrap(),
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
