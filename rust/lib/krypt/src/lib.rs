//! lib.rs --- Krypt Key Management Library
pub use keyutils;
pub mod ks;
// pub use secret_service::SecretService;
// pub use secret_service::EncryptionType;

//// Secret Service 
// pub async fn connect_ss() -> Result<(), Box<dyn Error>> {
// initialize secret service (dbus connection and encryption session)
// let ss = SecretService::connect(EncryptionType::Dh).await?;
// // get default collection
// let collection = ss.get_default_collection().await?;
// // create new item
// collection.create_item(
//     "test_label", // label
//     HashMap::from([("test", "test_value")]), // properties
//     b"test_secret", // secret
//     false, // replace item with same attributes
//     "text/plain" // secret content type
// ).await?;
//    Ok(())
// }

//// Tests
#[cfg(test)]
mod tests;
