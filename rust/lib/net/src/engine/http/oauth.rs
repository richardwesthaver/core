use crate::Result;
use oauth2::{
  basic::{BasicClient, BasicTokenType},
  AuthUrl, ClientId, ClientSecret, EmptyExtraTokenFields, RedirectUrl, TokenUrl,
};
pub use obj::Oauth2Config;

pub use oauth2::{
  reqwest::async_http_client, AuthorizationCode, CsrfToken, PkceCodeChallenge, Scope,
  StandardTokenResponse, TokenResponse,
};
use std::env;

pub fn client(cfg: Oauth2Config) -> BasicClient {
  // Environment variables (* = required):
  // *"CLIENT_ID"     "123456789123456789";
  // *"CLIENT_SECRET" "rAn60Mch4ra-CTErsSf-r04utHcLienT";
  //  "REDIRECT_URL"  "http://127.0.0.1:3000/authorized";
  //  "AUTH_URL"      "https://rwest.io/api/oauth2/authorize?response_type=code";
  //  "TOKEN_URL"     "https://rwest.io/api/oauth2/token";

  let client_id = env::var("CLIENT_ID").unwrap_or(cfg.client_id);
  let client_secret = env::var("CLIENT_SECRET").unwrap_or(cfg.client_secret);
  let redirect_url =
    env::var("REDIRECT_URL").unwrap_or(cfg.redirect_uris.get(0).unwrap().to_string());
  let auth_url = env::var("AUTH_URL").unwrap_or(cfg.auth_uri);
  let token_url = env::var("TOKEN_URL").unwrap_or(cfg.token_uri);

  BasicClient::new(
    ClientId::new(client_id),
    Some(ClientSecret::new(client_secret)),
    AuthUrl::new(auth_url).unwrap(),
    Some(TokenUrl::new(token_url).unwrap()),
  )
  .set_redirect_uri(RedirectUrl::new(redirect_url).unwrap())
}

pub async fn auth(
  cfg: Oauth2Config,
) -> Result<StandardTokenResponse<EmptyExtraTokenFields, BasicTokenType>> {
  let client = client(cfg);
  let (pkce_challenge, pkce_verifier) = PkceCodeChallenge::new_random_sha256(); //generate challenge
                                                                                // Generate the full authorization URL.
  let (auth_url, _csrf_token) = client
    .authorize_url(CsrfToken::new_random)
    // Set the desired scopes.
    .add_scope(Scope::new("read".to_string()))
    .add_scope(Scope::new("write".to_string()))
    // Set the PKCE code challenge.
    .set_pkce_challenge(pkce_challenge)
    .url();

  println!("point browser to: {}", auth_url);

  let tok = client
    .exchange_code(AuthorizationCode::new(
      "some authorization code".to_string(),
    ))
    // Set the PKCE code verifier.
    .set_pkce_verifier(pkce_verifier)
    .request_async(async_http_client)
    .await
    .unwrap();
  Ok(tok)
}
