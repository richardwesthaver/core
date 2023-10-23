//! network server primitives
use async_trait::async_trait;
pub use hyper;
use obj::NetworkConfig;
pub struct Server {
  pub cfg: NetworkConfig,
}

#[async_trait]
pub trait Serve {
  async fn run(&self);
}
