//! Auth Configs
use serde::{Deserialize, Serialize};

#[cfg(feature = "oauth")]
use yup_oauth2::ApplicationSecret;

#[derive(Serialize, Deserialize, Debug, Default, Hash)]
pub struct AuthConfig {
  pub provider: String,
  #[cfg(feature = "oauth")]
  pub oauth: Option<Oauth2Config>,
  pub ssh: Option<SshConfig>,
  pub pw: Option<PasswordConfig>,
}

#[derive(Serialize, Deserialize, Default, Debug, Hash)]
pub struct PasswordConfig(String, String);

#[cfg(feature = "oauth")]
#[derive(Serialize, Deserialize, Hash, Debug, PartialEq, Clone, Default)]
pub struct Oauth2Config {
  pub client_id: String,
  pub client_secret: String,
  pub redirect_uris: Vec<String>,
  pub auth_uri: String,
  pub token_uri: String,
  pub project_id: Option<String>, //for apptoken
  pub client_email: Option<String>,
  /// The URL of the public x509 certificate, used to verify the signature on
  /// JWTs, such as ID tokens, signed by the authentication provider.
  pub auth_provider_x509_cert_url: Option<String>,
  ///  The URL of the public x509 certificate, used to verify JWTs signed by the
  /// client.
  pub client_x509_cert_url: Option<String>,
}

#[cfg(feature = "oauth")]
impl From<ApplicationSecret> for Oauth2Config {
  fn from(shh: ApplicationSecret) -> Self {
    Oauth2Config {
      client_id: shh.client_id,
      client_secret: shh.client_secret,
      redirect_uris: shh.redirect_uris,
      auth_uri: shh.auth_uri,
      token_uri: shh.token_uri,
      project_id: shh.project_id,
      client_email: shh.client_email,
      auth_provider_x509_cert_url: shh.auth_provider_x509_cert_url,
      client_x509_cert_url: shh.client_x509_cert_url,
    }
  }
}

#[cfg(feature = "oauth")]
impl From<Oauth2Config> for ApplicationSecret {
  fn from(cfg: Oauth2Config) -> Self {
    ApplicationSecret {
      client_id: cfg.client_id,
      client_secret: cfg.client_secret,
      redirect_uris: cfg.redirect_uris,
      auth_uri: cfg.auth_uri,
      token_uri: cfg.token_uri,
      project_id: cfg.project_id,
      client_email: cfg.client_email,
      auth_provider_x509_cert_url: cfg.auth_provider_x509_cert_url,
      client_x509_cert_url: cfg.client_x509_cert_url,
    }
  }
}

#[derive(Serialize, Deserialize, Hash, Debug, PartialEq, Clone, Default)]
pub struct SshConfig {}
