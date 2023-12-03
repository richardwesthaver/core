use clap::{Parser, Subcommand};
use logger::{Logger,trace};
// use obj::Objective;
use std::path::PathBuf;
use util::Result;
use util::cli::log_level_str_from_cli;
// use jmap_client::{client::Client, core::query::Filter, email, mailbox};

#[derive(Debug, Parser)]
#[command(name="mailman",author, version, about, long_about = None)]
struct Cli {
  /// Command to run
  #[command(subcommand)]
  cmd: Option<Cmd>,
  /// Set the default config file
  #[arg(short, long, env = "MAILMAN_CONFIG_FILE")]
  cfg: Option<PathBuf>,
  /// Set a user for this command
  #[arg(short, long, env = "USER")]
  user: Option<String>,
  /// Set log level
  #[arg(short, long, action = clap::ArgAction::Count)]
  level: u8,
}

#[derive(Subcommand, Debug)]
enum Cmd {
  /// Ping the server
  Ping,
  /// Search for items
  Search,
  /// Import an account
  Import,
  /// Export an account
  Export,
}

#[tokio::main]
async fn main() -> Result<()> {
  // parse args
  let args = Cli::parse();
  // init logger
  Logger::try_with_str(log_level_str_from_cli(args.level))?.start()?;
  trace!("{:?}", args);
  // load config

  let args = Cli::parse();
  println!("{:?}", args.cmd);
  Ok(())
}
