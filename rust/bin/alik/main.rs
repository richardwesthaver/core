//! app/cli/alik/main.rs --- Alik

/// Code:
// use logger::log;
use alik::*;
use clap::{Parser, Subcommand};
use logger::{debug, error, trace, warn, Logger};
use std::path::PathBuf;
use util::{cli::log_level_str_from_cli, Result};
#[derive(Debug, Parser)]
#[command(name="alik",author, version, about, long_about = None)]
struct Cli {
  /// Command to run
  #[command(subcommand)]
  cmd: Option<Cmd>,
  /// Set the default config file
  #[arg(short, long, env = "ALIK_CONFIG_FILE")]
  cfg: Option<PathBuf>,
  /// Set log level
  #[arg(short, long, action = clap::ArgAction::Count)]
  level: u8,
}

#[derive(Debug, Subcommand)]
enum Cmd {
  /// start the Alik service
  Start { service: Option<String> },
  /// Show Alik info
  Show {
    /// What to show
    kind: Option<String>,
  },
  /// Ping remote services
  Ping {},
}

#[tokio::main]
async fn main() -> Result<()> {
  // parse args
  let args = Cli::parse();
  // init logger
  Logger::try_with_str(log_level_str_from_cli(args.level))?.start()?;
  trace!("{:?}", args);
  // load config
  let cfg = if let Some(path) = args.cfg {
    match AlikConfig::load(path.clone()) {
      // FIXME
      Ok(c) => c,
      Err(e) => {
        warn!("{path:?}: {e}, using default config");
        AlikConfig::default()
      }
    }
  } else {
    AlikConfig::default()
  };
  // initialize
  let alik = Alik::with_config(&cfg);
  debug!("{:?}", cfg);
  debug!("{:?}", alik);
  // run cmd
  if let Some(cmd) = args.cmd {
    match cmd {
      Cmd::Start { service: srv } => {
        // start_service().await;
        if let Some(s) = srv {
          match s.as_str() {
            "graphiql" => {
              graphql::start_graphiql("127.0.0.1:0").await;
              Ok(())
            }
            "http-proxy" => {
              start_http_proxy("127.0.0.1:0").await;
              Ok(())
            }
            _ => {
              error!("invalid service name");
              Ok(())
            }
          }
        } else {
          let http_proxy =
            tokio::spawn(async move { start_http_proxy("127.0.0.1:0").await });
          let graphiql = tokio::spawn(async move {
            graphql::start_graphiql("127.0.0.1:0").await
          });

          tokio::try_join!(graphiql, http_proxy)?;
          Ok(())
        }
      }
      Cmd::Ping {} => Ok(()),
      Cmd::Show { kind: _ } => Ok(()),
    }
  } else {
    Ok(())
  }
}
