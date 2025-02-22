use quinn::{ClientConfig, ServerConfig, TransportConfig};

pub struct Quic {
  pub transport: TransportConfig,
  pub client: Option<ClientConfig>,
  pub server: Option<ServerConfig>,
}
