//! net client/server library
//!
//! net is used for building client/server programs on any platform.
mod err;
pub use err::{Error, Result};

// mod codec;
mod client;
pub use client::Client;
mod server;
pub use server::Server;
pub mod coding;
pub mod connection;
mod engine;
#[cfg(feature = "axum")]
pub use axum;
#[cfg(feature = "dns")]
pub use engine::dns;
#[cfg(feature = "http")]
pub use engine::http;
#[cfg(feature = "quic")]
pub use engine::quic;
#[cfg(all(feature = "uds", unix))]
pub use engine::uds;
#[cfg(feature = "reqwest")]
pub use reqwest;
#[cfg(all(feature = "tun", linux))]
pub use tokio_tun as tun;
#[cfg(test)]
mod tests;
