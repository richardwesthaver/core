//! crypto modules
mod con;

#[cfg(test)]
mod tests;

use con::{PUBLIC_KEY_LENGTH, SECRET_KEY_LENGTH};

pub use salsa20::{self, XSalsa20};
//use ed25519::signature::Signature as _;
use ed25519_dalek::*;
use hex::FromHex;

pub fn gen_keypair<S: AsRef<[u8]>, P: AsRef<[u8]>>(secret_key: S, public_key: P) -> Keypair {
  let sec_bytes: Vec<u8> = FromHex::from_hex(secret_key).unwrap();
  let pub_bytes: Vec<u8> = FromHex::from_hex(public_key).unwrap();
  let secret: SecretKey = SecretKey::from_bytes(&sec_bytes[..SECRET_KEY_LENGTH]).unwrap();
  let public: PublicKey = PublicKey::from_bytes(&pub_bytes[..PUBLIC_KEY_LENGTH]).unwrap();

  Keypair {
    secret: secret,
    public: public,
  }
}
