//! util library
#[cfg(feature = "anyhow")]
pub use anyhow::{Context, Result};
#[cfg(feature = "bs")]
pub mod bs;
#[cfg(feature = "cli")]
pub mod cli;
pub mod path;
#[cfg(test)]
mod tests;
#[cfg(feature = "time")]
pub mod time;
#[cfg(feature = "url")]
pub use url::Url;
