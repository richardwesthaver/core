//! freesound -- freesound.org API client
//!
//! Freesound.org is a collaborative database of Creative Commons
//! Licensed sounds. It has a growing library of audio uploaded by
//! people around the world. It also provides an intricate API with
//! features such as basic search, upload, download, and even
//! fingerprint search based on analysis files from Essentia.
//!
//! This module implements the client-side freesound.org API.
//!
//! REF: <https://freesound.org/docs/api/>
//! ENDPOINT: <https://freesound.org/apiv2/>
use std::cmp::min;
use std::fmt;
use std::fs;
use std::path::Path;
use std::time::{SystemTime, Duration};
use tenex_util::indicatif::{ProgressBar, ProgressStyle};
use tenex_util::StreamExt;
use tenex_util::oauth2::{
  basic::{BasicClient, BasicTokenType},
  AuthUrl, AuthorizationCode, ClientId, ClientSecret, EmptyExtraTokenFields,
  RedirectUrl, RefreshToken, StandardTokenResponse, TokenResponse, TokenUrl,
};
use reqwest::{IntoUrl, RequestBuilder, Response, Url};
use serde::{Serialize, Deserialize};
use tokio::fs::File;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpListener;
use tenex_util::open_browser;
use reqwest::Client;

pub const FREESOUND_ENDPOINT: &str = "https://freesound.org/apiv2";

pub const USER_AGENT: &str =
  concat!("tenex/", env!("CARGO_PKG_VERSION"), " (https://rwest.io)");

pub const CONFIG_FILE: &str = "freesound.json";
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
  Http(reqwest::Error),
  Json(serde_json::Error),
  Io(std::io::Error),
  TokenExpired,
  TokenRefreshFailed,
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match *self {
      Error::Http(ref err) => Some(err),
      Error::Json(ref err) => Some(err),
      Error::Io(ref err) => Some(err),
      Error::TokenExpired => None,
      Error::TokenRefreshFailed => None,
    }
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Error::Http(ref err) => err.fmt(f),
      Error::Json(ref err) => err.fmt(f),
      Error::Io(ref err) => err.fmt(f),
      Error::TokenExpired => f.write_str("Refresh token has expired"),
      Error::TokenRefreshFailed => {
        f.write_str("Failed to renew auth with refresh token")
      }
    }
  }
}

impl From<reqwest::Error> for Error {
  fn from(err: reqwest::Error) -> Error {
    Error::Http(err)
  }
}

impl From<serde_json::Error> for Error {
  fn from(err: serde_json::Error) -> Error {
    Error::Json(err)
  }
}

impl From<std::io::Error> for Error {
  fn from(err: std::io::Error) -> Error {
    Error::Io(err)
  }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct FreesoundConfig {
  pub client_id: Option<String>,
  pub client_secret: Option<String>,
  pub redirect_url: Option<String>,
  pub access_token: Option<String>,
  pub refresh_token: Option<String>,
  pub scopes: Option<Vec<String>>,
  pub expires: Option<u64>,
}

impl FreesoundConfig {
  pub fn update(
    &mut self,
    access_token: &str,
    refresh_token: &str,
    expires_in: Duration,
    scopes: &[String],
  ) {
    self.access_token = Some(access_token.to_string());
    self.refresh_token = Some(refresh_token.to_string());
    self.scopes = Some(scopes.to_vec());
    let expires = SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
      .expect("SystemTime is before UNIX_EPOCH!?")
      + expires_in;
    self.expires = Some(expires.as_secs());
  }
  pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
    let content = fs::read(path)?;
    let config: FreesoundConfig = serde_json::from_slice(&content)?;
    Ok(config)
  }
}

pub async fn write_sound<P: AsRef<Path>>(
  res: Response,
  dst: P,
  progress: bool,
) -> Result<()> {
  let size = res.content_length().unwrap();
  let mut dst = File::create(dst.as_ref()).await.unwrap();
  let mut downloaded: u64 = 0;
  let pb = if progress {
    Some(
      ProgressBar::new(size)
	.with_style(ProgressStyle::default_spinner().template("{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {bytes}/{total_bytes} ({bytes_per_sec}, {eta})").unwrap()
		    .progress_chars("#>-"))
    )
  } else {
    None
  };

  let mut stream = res.bytes_stream();
  while let Some(b) = stream.next().await {
    let chunk = b.unwrap();
    dst.write_all(&chunk).await.unwrap();
    let new = min(downloaded + (chunk.len() as u64), size);
    downloaded = new;
    if let Some(pb) = &pb {
      pb.set_position(new);
    }
  }
  dst.flush().await.unwrap();
  if let Some(pb) = &pb {
    pb.finish();
  }
  Ok(())
}

#[derive(Debug, Default)]
pub struct FreeSoundClient {
  pub client: Client,
  pub cfg: FreesoundConfig,
}

impl FreeSoundClient {
  /// Create a new FreeSoundClient.
  ///
  /// Note: freesound.org is an authenticated API and thus requires
  /// the CFG field to be populated. Calls to the API will fail if
  /// required CFG fields aren't updated. Prefer `new_with_config`
  /// method for initialization.
  pub fn new() -> FreeSoundClient {
    FreeSoundClient {
      client: Client::new(),
      cfg: FreesoundConfig {
        redirect_url: Some("http://localhost:8080".to_string()),
        ..Default::default()
      },
    }
  }

  /// Create a new FreeSoundClient with the given CFG.
  pub fn new_with_config(cfg: &FreesoundConfig) -> Self {
    FreeSoundClient {
      client: Client::new(),
      cfg: cfg.to_owned(),
    }
  }

  /// Update the net.freesound fields of a GlobalConfig.
  pub fn save<P: AsRef<Path>>(&self, path: P) -> Result<()> {
    let json = serde_json::to_string_pretty(&self.cfg)?;
    let mut path = path.as_ref().to_path_buf();
    if path.is_dir() {
      path = path.join(CONFIG_FILE);
    }
    fs::write(path, json)?;
    Ok(())
  }

  pub fn auth_client(&self) -> BasicClient {
    let client_id = ClientId::new(self.cfg.client_id.as_ref().unwrap().clone());
    let client_secret =
      ClientSecret::new(self.cfg.client_secret.as_ref().unwrap().clone());
    let auth_url = AuthUrl::new(format!(
      "{}/oauth2/authorize/?client_id={}&response_type=code",
      FREESOUND_ENDPOINT,
      self.cfg.client_id.as_ref().unwrap().clone()
    ))
    .unwrap();
    let token_url =
      TokenUrl::new(format!("{}/oauth2/access_token/", FREESOUND_ENDPOINT)).unwrap();
    BasicClient::new(client_id, Some(client_secret), auth_url, Some(token_url))
      .set_redirect_uri(
        RedirectUrl::new(self.cfg.redirect_url.as_ref().unwrap().clone()).unwrap(),
      )
  }

  pub fn update_cfg(
    &mut self,
    token: StandardTokenResponse<EmptyExtraTokenFields, BasicTokenType>,
  ) {
    self.cfg.update(
      token.access_token().secret().as_str(),
      token.refresh_token().unwrap().secret().as_str(),
      token.expires_in().unwrap(),
      &token
        .scopes()
        .unwrap()
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>(),
    );
  }

  /// Do the Oauth2 Dance as described in
  /// https://freesound.org/docs/api/authentication.html#oauth2-authentication
  ///
  /// Step 1: user is redirected to a Freesound page where they log in
  /// and are asked to give permission to MPK.
  ///
  /// Step 2: If user grants access, Freesound redirects to the
  /// REDIRECT_URL with an authorization grant as a GET parameter.
  ///
  /// Step 3: MPK uses that authorization grant to request an access
  /// token that 'links' user with MPK and that needs to be added to
  /// all API requests.
  ///
  /// Note: all requests using OAuth2 API need to be made over HTTPS.
  pub async fn auth(&mut self, auto: bool) -> Result<()> {
    if self.refresh_auth().await.is_ok() {
      println!("token refresh successful");
      Ok(())
    } else {
      let client = self.auth_client();
      let client_id = self.cfg.client_id.as_ref().unwrap();
      println!(
        "go to: {}/oauth2/authorize/?client_id={}&response_type=code",
        FREESOUND_ENDPOINT, client_id
      );
      if auto {
        open_browser(
          format!(
            "{}/oauth2/authorize/?client_id={}&response_type=code",
            FREESOUND_ENDPOINT, client_id
          )
          .as_str(),
        );
      }
      let listener = TcpListener::bind("localhost:8080").await.unwrap();
      loop {
        if let Ok((mut stream, _)) = listener.accept().await {
          let mut reader = BufReader::new(&mut stream);
          let mut line = String::new();
          reader.read_line(&mut line).await.unwrap();
          let redirect_url = line.split_whitespace().nth(1).unwrap();
          let url =
            Url::parse(&("http://localhost:8080".to_string() + redirect_url)).unwrap();

          let code_pair = url
            .query_pairs()
            .find(|pair| {
              let &(ref key, _) = pair;
              key == "code"
            })
            .unwrap();

          let (_, value) = code_pair;
          let code = AuthorizationCode::new(value.into_owned());
          println!("got code: {:?}", code.secret());
          // let state_pair = url
          //   .query_pairs()
          //   .find(|pair| {
          //     let &(ref key, _) = pair;
          //     key == "state"
          //   })
          //   .unwrap();

          // let (_, value) = state_pair;
          // state = CsrfToken::new(value.into_owned());
          let message = "Go back to your terminal :)";
          let response = format!(
            "HTTP/1.1 200 OK\r\ncontent-length: {}\r\n\r\n{}",
            message.len(),
            message
          );
          stream.write_all(response.as_bytes()).await.unwrap();
          let token_res = client
            .exchange_code(code)
            .request_async(tenex_util::oauth2::reqwest::async_http_client)
            .await
            .unwrap();

          self.update_cfg(token_res);
          break Ok(());
        }
      }
    }
  }

  pub async fn refresh_auth(&mut self) -> Result<()> {
    if let Some(d) = self.cfg.expires {
      let exp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("SystemTime is before UNIX_EPOCH!?")
        .as_secs();
      if exp < d {
        let client = self.auth_client();
        if let Some(t) = &self.cfg.refresh_token {
          let token_res = client
            .exchange_refresh_token(&RefreshToken::new(t.to_string()))
            .request_async(tenex_util::oauth2::reqwest::async_http_client)
            .await
            .unwrap();
          self.update_cfg(token_res);
          return Ok(());
        } else {
          return Err(Error::TokenRefreshFailed);
        }
      } else {
        return Err(Error::TokenExpired);
      }
    }
    Ok(())
  }

  pub async fn request<'a>(&self, req: FreeSoundRequest<'a>) -> Result<Response> {
    let res = self
      .client
      .execute(
        req
          .request(&self.client)
          .bearer_auth(self.cfg.access_token.as_ref().unwrap())
          .build()?,
      )
      .await?;
    Ok(res)
  }

  pub async fn get_raw<U: IntoUrl>(&self, url: U) -> Result<Response> {
    let res = self
      .client
      .get(url)
      .bearer_auth(self.cfg.access_token.as_ref().unwrap())
      .send()
      .await?;
    Ok(res)
  }
}

pub enum FreeSoundRequest<'a> {
  SearchText {
    query: &'a str,
    filter: Option<&'a str>,
    sort: &'a str,
    group_by_pack: bool,
    weights: &'a str,
    page: usize,
    /// max = 150
    page_size: u8,
    fields: &'a [&'a str],
    descriptors: &'a [&'a str],
    normalized: bool,
  },
  SearchContent,
  SearchCombined,
  Sound {
    id: u64,
  },
  SoundAnalysis {
    id: u64,
  },
  SoundSimilar {
    id: u64,
  },
  SoundComments {
    id: u64,
  },
  SoundDownload {
    id: u64,
  },
  SoundUpload,
  SoundDescribe,
  SoundPendingUpload,
  SoundEdit,
  SoundBookmark {
    id: u64,
    name: &'a str,
    category: &'a str,
  },
  SoundRate,
  SoundComment,
  User,
  UserSounds,
  UserPacks,
  UserBookmarkCategories,
  UserBookmarkCategorySounds,
  Pack,
  PackSounds,
  PackDownload,
  Me,
  Descriptors,
}

impl<'a> FreeSoundRequest<'a> {
  pub fn request(&self, client: &Client) -> RequestBuilder {
    client.get(self.addr()).query(self.params().as_slice())
  }

  pub fn addr(&self) -> String {
    let slug = match self {
      FreeSoundRequest::SearchText { .. } => "/search/text".to_string(),
      FreeSoundRequest::SearchContent => "/search/content".to_string(),
      FreeSoundRequest::SearchCombined => "/search/combined".to_string(),
      FreeSoundRequest::Sound { ref id } => format!("/sounds/{}", id),
      FreeSoundRequest::SoundAnalysis { ref id } => format!("/sounds/{}/analysis", id),
      FreeSoundRequest::SoundSimilar { ref id } => format!("/sounds/{}/similar", id),
      FreeSoundRequest::SoundComments { ref id } => format!("/sounds/{}/comments", id),
      FreeSoundRequest::SoundDownload { ref id } => format!("/sounds/{}/download", id),
      _ => "".to_string(),
    };
    format!("{}{}", FREESOUND_ENDPOINT, slug)
  }

  pub fn params(&self) -> Vec<(String, String)> {
    match self {
      FreeSoundRequest::SearchText {
        query,
        filter,
        sort,
        group_by_pack,
        weights,
        page,
        page_size,
        fields,
        descriptors,
        normalized,
      } => {
        let gbp = if *group_by_pack { "1" } else { "0" }.to_string();
        let normalized = if *normalized { "1" } else { "0" }.to_string();
        let mut params = vec![
          ("query".to_string(), query.to_string()),
          ("sort".to_string(), sort.to_string()),
          ("group_by_pack".to_string(), gbp),
          ("normalized".to_string(), normalized),
          ("weights".to_string(), weights.to_string()),
          ("fields".to_string(), fields.join(",")),
          ("descriptors".to_string(), descriptors.join(",")),
          ("page".to_string(), page.to_string()),
          ("page_size".to_string(), page_size.to_string()),
        ];
        if let Some(f) = filter {
          params.push(("filter".to_string(), f.to_string()));
        }
        params
      }
      _ => {
        vec![]
      }
    }
  }
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum FreeSoundResponse {
  SearchText {
    count: usize,
    next: Option<Url>,
    results: Vec<FreeSoundSearchResult>,
    previous: Option<Url>,
  },
}

impl FreeSoundResponse {
  pub async fn parse(res: Response) -> FreeSoundResponse {
    res.json::<FreeSoundResponse>().await.unwrap()
  }
}

impl fmt::Display for FreeSoundResponse {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      FreeSoundResponse::SearchText {
        count,
        next,
        results,
        previous,
      } => {
        let next = next
          .as_ref()
          .map(|u| u.to_string())
          .unwrap_or_else(|| "null".to_string());
        let previous = previous
          .as_ref()
          .map(|u| u.to_string())
          .unwrap_or_else(|| "null".to_string());
        let res: String = results
          .iter()
          .map(|r| r.to_string())
          .collect::<Vec<String>>()
          .join("\n");
        f.write_str(
          format!(
            "count: {}, results: [\n{}\n], next: {}, previous: {}",
            count, res, next, previous
          )
          .as_str(),
        )
      }
    }
  }
}

#[derive(Deserialize, Debug)]
pub struct FreeSoundSearchResult {
  pub id: Option<u64>,
  pub name: Option<String>,
  pub tags: Option<Vec<String>>,
  pub license: Option<Url>,
}

impl fmt::Display for FreeSoundSearchResult {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(n) = &self.id {
      write!(f, "{}, ", n)?
    };
    if let Some(s) = &self.name {
      write!(f, "name: {} ", s)?
    };
    if let Some(v) = &self.tags {
      write!(f, "tags: {}, ", v.join(":"))?
    };
    if let Some(s) = &self.license {
      write!(f, "license: {}, ", s)?
    };
    Ok(())
  }
}
