/// app/cli/alik/lib.rs --- Alik Lib

// Helper of man

/// Code:
pub mod graphql;
pub mod ping;
pub mod http;
pub mod udp;

use db::{Db, DbConfigExt, rocksdb};
use net::{
  http::tower::trace::TraceLayer,
  reqwest::Client,
  axum::{
    body::{Body, Bytes},
    extract::State,
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode},
    response::{IntoResponse, Response},
    routing::get,
    Router,
  },
};

use serde::{Deserialize, Serialize};

use std::{
  sync::Arc,
  collections::HashMap,
  fs,
  path::{Path, PathBuf},
};

pub use krypt::KryptConfig;

use logger::{info, log, tracing::Span};

use obj::{Configure, NetworkConfig, Objective};

use std::time::Duration;

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct AlikConfig {
  krypt: KryptConfig,
  db_path: Option<PathBuf>,
  db_opts: HashMap<String, String>,
  net: NetworkConfig,
}
impl AlikConfig {
  pub fn new() -> Self {
    AlikConfig::default()
  }
  pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, obj::Error> {
    let s = fs::read_to_string(path)?;
    AlikConfig::from_json_str(&s)
  }
}

obj::impl_config!(AlikConfig);

impl DbConfigExt for AlikConfig {
  fn db_path(self) -> Option<PathBuf> {
    self.db_path
  }
  fn db_user(self) -> Option<String> {
    None
  }
  fn set_db_config_value(&mut self, key: &str, val: &str) -> Option<String> {
    self.db_opts.insert(key.to_string(), val.to_string())
  }
  fn get_db_config_value(self, key: &str) -> Option<String> {
    self.db_opts.get(key).cloned()
  }
}

pub trait AlikService {}

#[derive(Debug)]
pub struct Alik {
  db: Option<rocksdb::DB>,
  config: Arc<AlikConfig>,
  router: Option<Router>,
}

impl Alik {
  pub fn new() -> Alik {
    Alik {
      db: None,
      config: Arc::new(AlikConfig::new()),
      router: None,
    }
  }
  pub fn with_config(cfg: &AlikConfig) -> Alik {
    Alik {
      db: None,
      config: Arc::new(cfg.to_owned()),
      router: None,
    }
  }
  pub fn network_init(&mut self) -> Result<(), net::Error> {
    let socket = &self.config.net.socket;
    let peers = self.config.net.peers.as_ref();
    info!("initializing on socket {:?}", socket);
    info!("initializing with peers {:?}", peers.unwrap());
    self.router = Some(Router::new().route("/", get("")));
    Ok(())
  }
}

impl Db for Alik {
  fn db_init(&self) -> Result<rocksdb::DB, db::Error> {
    let path = self.config.db_path.as_ref();
    let opts = &self.config.db_opts;
    info!("{:?}",opts);
    rocksdb::DB::open(&rocksdb::Options::default(),path.unwrap()).unwrap();
    Ok(rocksdb::DB::open_default("").unwrap())
  }
  fn db_init_mut(&mut self) -> Result<(), db::Error> {
    self.db = Some(self.db_init().unwrap());
    Ok(())
  }
  fn db_open(&self) -> Result<(), db::Error> {
    Ok(())
  }
  fn db_close(&self) -> Result<(), db::Error> {
    if let Some(db) = &self.db {
      db.cancel_all_background_work(true)
    };
    Ok(())
  }
  fn db_close_mut(&mut self) -> Result<(), db::Error> {
    self.db_close().unwrap();
    self.db = None;
    Ok(())
  }
  fn db_query(&self) -> Result<(), db::Error> {
    Ok(())
  }
  fn db_transaction(&self) -> Result<(), db::Error> {
    Ok(())
  }
}

/// Server
pub async fn proxy_via_reqwest(State(client): State<Client>) -> Response {
  let reqwest_response =
    match client.get("https://compiler.company").send().await {
      Ok(res) => res,
      Err(err) => {
        log::error!("{} {}", &err, "request failed");
        return (StatusCode::BAD_REQUEST, Body::empty()).into_response();
      }
    };

  let response_builder =
    Response::builder().status(reqwest_response.status().as_u16());

  // different http crate versions?
  let mut headers = HeaderMap::with_capacity(reqwest_response.headers().len());
  headers.extend(reqwest_response.headers().into_iter().map(
    |(name, value)| {
      let name = HeaderName::from_bytes(name.as_ref()).unwrap();
      let value = HeaderValue::from_bytes(value.as_ref()).unwrap();
      (name, value)
    },
  ));

  response_builder
    .body(Body::from_stream(reqwest_response.bytes_stream()))
    // body is empty, no error
    .unwrap()
}

pub async fn start_http_proxy(addr: &str) {
  let client = Client::new();
  let app = Router::new()
    .route("/", get(proxy_via_reqwest))
    .layer(TraceLayer::new_for_http().on_body_chunk(
      |chunk: &Bytes, _latency: Duration, _span: &Span| {
        log::debug!("streaming {} bytes", chunk.len());
      },
    ))
    .with_state(client);
  let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
  println!(
    "http_proxy running on: http://{}",
    listener.local_addr().unwrap()
  );
  net::axum::serve(listener, app).await.unwrap();
}

// pub async fn stream_some_data() -> Body {
//   let stream = net::stream::iter(0..5)
//     .throttle(Duration::from_secs(1))
//     .map(|n| n.to_string())
//     .map(Ok::<_, Infallible>);
//   Body::from_stream(stream)
// }
