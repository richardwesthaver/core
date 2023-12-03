use secret_service::Error;
pub use secret_service::{EncryptionType, SecretService};
//// Secret Service
pub async fn connect_ss<'a>() -> Result<SecretService<'a>, Error> {
  // initialize secret service (dbus connection and encryption session)
  SecretService::connect(EncryptionType::Dh).await
}
