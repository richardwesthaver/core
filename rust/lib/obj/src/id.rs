//! rlib/obj/src/id.rs --- rlib::obj::id
//!
//! primitive ID types.

pub use hash::Id;
pub use rusty_ulid::{self, Ulid};
use std::{fmt, str::FromStr};
pub use uuid::Uuid;
/// Identity trait
///
/// Defines Identity-related behaviors
pub trait Identity: Sized {
  /// return the hashed bytes of an ObjectId
  fn id(&self) -> Id;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ObjectId(u128);

pub struct NameSpace {
  pub prefix: Option<String>,
  pub capacity: u64,
  pub route: Vec<Id>,
  pub key: Option<Id>,
}

pub struct Domain {
  pub ns: NameSpace,
  pub id: Id,
}

impl From<Uuid> for ObjectId {
  fn from(uuid: Uuid) -> Self {
    ObjectId(uuid.as_u128())
  }
}

impl From<Ulid> for ObjectId {
  fn from(ulid: Ulid) -> Self {
    ObjectId(u128::from(ulid))
  }
}

impl From<u128> for ObjectId {
  fn from(src: u128) -> Self {
    ObjectId(src)
  }
}

impl FromStr for ObjectId {
  type Err = ();
  fn from_str(input: &str) -> std::result::Result<ObjectId, Self::Err> {
    match input {
      i => Ok(ObjectId(u128::from(Ulid::from_str(i).unwrap()))),
    }
  }
}

impl fmt::Display for ObjectId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      ObjectId(i) => {
        write!(f, "{}", Ulid::from(i))
      }
    }
  }
}
