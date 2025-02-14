//! network server primitives
pub use hyper;
use obj::NetworkConfig;
pub struct Server {
  pub cfg: NetworkConfig,
}

#[trait_variant::make(Serve: Send)]
pub trait LocalServe {
  async fn run(&self);
}
