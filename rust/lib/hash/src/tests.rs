use crate::{Hasher, OutputReader};
use hex::decode;
use std::convert::TryInto;

#[test]
fn hex_hash() -> Result<(), Box<dyn std::error::Error>> {
    let mut hasher1 = crate::Hasher::new();
    hasher1.update(b"foo");
    hasher1.update(b"bar");
    hasher1.update(b"baz");
    let out1 = hasher1.finalize();
    let mut xof1 = [0; 301];
    hasher1.finalize_xof().fill(&mut xof1);
    assert_eq!(out1.as_bytes(), &xof1[..32]);

    let hash_hex = "d74981efa70a0c880b8d8c1985d075dbcbf679b99a5f9914e5aaf96b831a9e24";
    let hash_bytes = decode(hash_hex).unwrap();
    let hash_array: [u8; blake3::OUT_LEN] = hash_bytes[..].try_into().unwrap();
    let hash: blake3::Hash = hash_array.into();
    Ok(())
}
