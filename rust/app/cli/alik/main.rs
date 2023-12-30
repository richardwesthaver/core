//! app/cli/alik/main.rs --- Alik

//// Code:
use net::axum::http::{HeaderMap, StatusCode};
use net::axum::{
    body::{Body, Bytes},
  extract::State,
    http::{HeaderName, HeaderValue},
    response::{IntoResponse,Response},
  // routing::get,
    Router,
};
use logger::log;
use net::reqwest::Client;
use net::stream::StreamExt;
use clap::{Parser, Subcommand};
use krypt::{keyutils::keytypes, ks, ss, KryptConfig};
use logger::{debug, info, trace, warn, Logger};
use obj::Objective;
use std::path::PathBuf;
use std::{convert::Infallible, time::Duration};
use util::{cli::log_level_str_from_cli, Result};
//// Server
async fn proxy_via_reqwest(State(client): State<Client>) -> Response {
    let reqwest_response = match client.get("http://127.0.0.1:3000/stream").send().await {
        Ok(res) => res,
        Err(err) => {
            log::error!(%err, "request failed");
            return (StatusCode::BAD_REQUEST, Body::empty()).into_response();
        }
    };

    let response_builder = Response::builder().status(reqwest_response.status().as_u16());

    // Here the mapping of headers is required due to reqwest and axum differ on the http crate versions
    let mut headers = HeaderMap::with_capacity(reqwest_response.headers().len());
    headers.extend(reqwest_response.headers().into_iter().map(|(name, value)| {
        let name = HeaderName::from_bytes(name.as_ref()).unwrap();
        let value = HeaderValue::from_bytes(value.as_ref()).unwrap();
        (name, value)
    }));

    response_builder
        .body(Body::wrap_stream(reqwest_response.bytes_stream()))
        // This unwrap is fine because the body is empty here
        .unwrap()
}

async fn stream_some_data() -> Body {
    let stream = net::stream::iter(0..5)
        .throttle(Duration::from_secs(1))
        .map(|n| n.to_string())
        .map(Ok::<_, Infallible>);
    Body::wrap_stream(stream)
}

#[derive(Debug, Parser)]
#[command(name="alik",author, version, about, long_about = None)]
struct Cli {
  /// Command to run
  #[command(subcommand)]
  cmd: Option<Cmd>,
  /// Set the default config file
  #[arg(short, long, env = "ALIK_CONFIG_FILE")]
  cfg: Option<PathBuf>,
  /// Set a user for this command
  #[arg(short, long, env = "USER")]
  user: Option<String>,
  /// Set log level
  #[arg(short, long, action = clap::ArgAction::Count)]
  level: u8,
}

#[derive(Debug, Subcommand)]
enum Cmd {
  /// start the Alik proxy server
  Start {},
  /// Show Alik info
  Show {
    /// What to show
    kind: Option<String>,
  },
}

#[tokio::main]
async fn main() -> Result<()> {
  // parse args
  let args = Cli::parse();
  // init logger
  Logger::try_with_str(log_level_str_from_cli(args.level))?.start()?;
  trace!("{:?}", args);
  // load config
  // let cfg = if let Some(path) = args.cfg {
  //   match AlikConfig::load_file(path.clone()) {
  //     // FIXME
  //     Ok(c) => c,
  //     Err(e) => {
  //       warn!("{path:?}: {e}, using default config");
  //       KryptConfig::default()
  //     }
  //   }
  // } else {
  //   KryptConfig::default()
  // };
  // debug!("{:?}", cfg);
  // run cmd
  if let Some(cmd) = args.cmd {
    match cmd {
      Cmd::Start {} => Ok(()),
      Cmd::Show { kind } => Ok(()),
    }
  }else {
    Ok(())
  }
}

