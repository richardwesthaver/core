use clap::{Parser, Subcommand};
use krypt::{keyutils::keytypes, ks, ss, KryptConfig};
use logger::{debug, info, trace, warn, Logger};
use obj::Objective;
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
      Cmd::Start {} => {
        nyi!();
      }
      Cmd::Show { kind } => { Ok(()) }
    } else {
      Ok(())
    }
  }
}
