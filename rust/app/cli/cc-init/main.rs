use clap::Parser;
use dl::{download_to_path_with_backend, Backend, TlsBackend};
use logger::{info, trace, Logger};
use std::{env, path::Path};
use util::{cli::log_level_str_from_cli, Result, Url};

pub const ABOUT: &str = concat!(
  "cc-init ",
  env!("CORE_VERSION"),
  " (",
  env!("CORE_TARGET"),
  ")"
);

pub const PACKY_ROOT: &str = "https://packy.compiler.company/";

#[derive(Debug, Parser)]
#[command(name="cc-init",author, version=env!("CORE_VERSION"), about=ABOUT)]
struct Cli {
  /// Disable confirmation prompts
  #[arg(short)]
  y: bool,
  /// Set the log level
  #[arg(short, long, action = clap::ArgAction::Count)]
  level: u8,
}

fn dl<P: AsRef<Path> + std::fmt::Debug>(url: &str, dst: P) -> Result<()> {
  info!("downloading {url} -> {dst:?}");
  let url = Url::parse(url)?;
  download_to_path_with_backend(
    Backend::Reqwest(TlsBackend::Default),
    &url,
    dst.as_ref(),
    true,
    None,
  )
}

fn main() -> Result<()> {
  let args = Cli::parse();
  // init logger
  Logger::try_with_str(log_level_str_from_cli(args.level))?.start()?;
  trace!("{:?}", args);
  println!("Welcome to The Compiler Company.");
  // dl("http://google.com", "dl")?;
  info!("detecting system...");
  
  Ok(())
}
