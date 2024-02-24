//! app/cli/alik/lib.rs --- Alik Lib

use db::{Db, DbConfigExt};
/// Code:
// use net::axum::Router;
use net::{
  axum::{
    body::{Body, Bytes},
    extract::State,
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode},
    response::{self, IntoResponse, Response},
    routing::get,
    Router,
  },
  http::graphql::http::GraphiQLSource,
};
use serde::{Deserialize, Serialize};
use std::{
  collections::HashMap,
  fs,
  path::{Path, PathBuf},
};
use tokio::net::TcpListener;
// use net::http::graphql::http::{EmptyMutation, EmptySubscription, Schema};
// use net::http::graphql_axum::GraphQL;
pub use krypt::KryptConfig;
use logger::{
  log,
  tracing::{self, Span},
};
use net::{http::tower::trace::TraceLayer, reqwest::Client};
use obj::{Configure, Objective};
use std::time::Duration;
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct AlikConfig {
  krypt: KryptConfig,
  db_path: Option<PathBuf>,
  db_opts: HashMap<String, String>,
}
impl AlikConfig {
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

#[derive(Debug)]
pub struct Alik {}

impl Alik {
  pub fn new() -> Alik {
    Alik {}
  }
  pub fn with_config(_cfg: AlikConfig) -> Alik {
    Alik {}
  }
}

impl Db for Alik {
  fn db_init(&self) -> Result<(), db::Error> {
    Ok(())
  }
  fn db_open(&self) -> Result<(), db::Error> {
    Ok(())
  }
  fn db_close(&self) -> Result<(), db::Error> {
    Ok(())
  }
  fn db_query(&self) -> Result<(), db::Error> {
    Ok(())
  }
  fn db_transaction(&self) -> Result<(), db::Error> {
    Ok(())
  }
}

pub async fn graphiql() -> impl IntoResponse {
  response::Html(GraphiQLSource::build().endpoint("/").finish())
}

pub async fn start_graphiql(addr: &str) {
  // let schema = Schema::build(QueryRoot, EmptyMutation, EmptySubscription)
  //     .data(Vec::new())
  //     .finish();

  let app = Router::new().route("/", get(graphiql));
  // .post_service(GraphQL::new(schema)));

  let listener = TcpListener::bind(addr).await.unwrap();
  println!(
    "graphiql running on: http://{}",
    listener.local_addr().unwrap()
  );
  net::axum::serve(TcpListener::bind(addr).await.unwrap(), app)
    .await
    .unwrap();
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
        tracing::debug!("streaming {} bytes", chunk.len());
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
