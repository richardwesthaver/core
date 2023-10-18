//! lib.rs --- S-eXPressions.
#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;

mod err;
#[doc(inline)]
pub use err::{Error, Result};

mod io;

macro_rules! e {
  ($e:expr $(,)?) => {
    match $e {
      core::result::Result::Ok(val) => val,
      core::result::Result::Err(err) => return core::result::Result::Err(err),
    }
  };
}

// pub mod de;
// pub use crate::de::{
//   from_reader, from_slice, from_str, from_traits, Deserializer,
// };

mod fmt;
#[doc(inline)]
pub use fmt::{DefaultFormatter, Formatter, ReadFormatter, WriteFormatter};
pub mod form;
pub mod iter;
pub mod macs;
pub mod read;
pub mod ser;
pub mod tok;
#[doc(inline)]
pub use form::Form;
#[doc(inline)]
pub use macs::{Macro, MacroObject, ReadTable, WriteTable};
#[doc(inline)]
pub use ser::{to_string, to_vec, to_writer, Serializer};
#[doc(inline)]
pub use tok::Token;

#[macro_export(local_inner_macros)]
macro_rules! sxp {
  ($($sxp:tt)+) => {
    _sxp!($($sxp)+)
  };
}

// may not be necessary
#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! _sxp {
  (@array [$($elems:expr),*]) => {
    sxp_vec![$($elems),*]
  };
  // Next element is `nil`.
  (@array [$($elems:expr,)*] nil $($rest:tt)*) => {
    _sxp!(@array [$($elems,)* _sxp!(nil)] $($rest)*)
  };
  // Unexpected token after most recent element.
  (@array [$($elems:expr),*] $unexpected:tt $($rest:tt)*) => {
    sxp_unexpected!($unexpected)
  };

  //////////////////////////////////////////////////////////////////////////
  // TT muncher for parsing the inside of a object (:foo "bar"). Each entry is
  // inserted into the given map variable.
  //
  // Must be invoked as: _sxp!(@object $map () ($($tt)*) ($($tt)*))
  //
  // We require two copies of the input tokens so that we can match on one
  // copy and trigger errors on the other copy.
  //////////////////////////////////////////////////////////////////////////

  // Done.
  (@object $object:ident () () ()) => {};

  // Insert the current entry.
  (@object $object:ident ( [$($key:tt)+] ($value:expr) ) $($rest:tt)*) => {
    let _ = $object.insert(($($key)+).into(), $value);
    _sxp!(@object $object () ($($rest)*) ($($rest)*));
    };

    // Current entry followed by unexpected token.
    (@object $object:ident [$($key:tt)+] ($value:expr) $unexpected:tt $($rest:tt)*) => {
        sxp_unexpected!($unexpected);
    };

    // Insert the last entry
    (@object $object:ident [$($key:tt)+] ($value:expr)) => {
        let _ = $object.insert(($($key)+).into(), $value);
    };

    // Next value is `nil`.
    (@object $object:ident ($($key:tt)+) (nil $($rest:tt)*) $copy:tt) => {
        json_internal!(@object $object [$($key)+] (json_internal!(null)) $($rest)*);
    };

}
