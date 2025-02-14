//! io.rs --- minimal nostd-friendly `std::io` wrapper
pub use self::imp::{Error, Result, Write};

#[cfg(not(feature = "std"))]
#[path = "nostd.rs"]
mod imp;

#[cfg(feature = "std")]
use std::io as imp;

#[cfg(feature = "std")]
pub use std::io::{Bytes, Read};
