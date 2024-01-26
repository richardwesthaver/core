use clap::Parser;
// use dl::{download_to_path_with_backend, Backend, TlsBackend};
use logger::{info, trace, Logger};
use std::env;
use std::path::{Path,PathBuf};
use util::{cli::log_level_str_from_cli, Result};
use util::Url;
pub const ABOUT: &str = concat!(
  "cc-install ",
  env!("CORE_VERSION"),
  " (",
  env!("CORE_TARGET"),
  ")"
);

pub const PACKY_URL: &str= "https://packy.compiler.company/";

#[derive(Debug, Parser)]
#[command(name="cc-install",author, version=env!("CORE_VERSION"), about=ABOUT)]
struct Cli {
  /// Disable confirmation prompts
  #[arg(short)]
  y: bool,
  /// Set the log level
  #[arg(short, long, action = clap::ArgAction::Count)]
  level: u8,
}

#[derive(Debug)]
enum CcPack {
  Core,
  RocksDb,
  Emacs,
  EmacsMini,
  Sbcl,
  Rust,
  Nushell,
  Tools,
  Demo,
  Source,
  Docs,
}

impl CcPack {
  fn slug(&self) -> &str {
    match *self {
      CcPack::RocksDb => "rocksdb-binary.tar.zst",
      CcPack::Emacs => "emacs-binary.tar.zst",
      CcPack::EmacsMini => "emacs-mini-binary.tar.zst",
      CcPack::Tools => "tools-binary.tar.zst",
      CcPack::Nushell => "nushell-binary.tar.zst",
      CcPack::Rust => "rust-binary.tar.zst",
      CcPack::Sbcl => "sbcl-binary.tar.zst",
      CcPack::Core => "core.tar.zst",
      CcPack::Demo => "demo.tar.zst",
      CcPack::Source => "source.tar.zst",
      CcPack::Docs => "docs.tar.zst",
    }
  }
}

#[derive(Debug)]
struct CcPacks(Vec<CcPack>);

impl std::fmt::Display for CcPacks {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f,"{:?}",self.0)
  }
}
  

impl Default for CcPacks {
  fn default() -> CcPacks {
    CcPacks(
      vec![
        CcPack::Core,
        CcPack::RocksDb,
        CcPack::Emacs,
        CcPack::Sbcl,
        CcPack::Rust,
        CcPack::Nushell,
        CcPack::Tools,
      ])
  }
}

#[derive(Debug)]
struct Installer {
  // directory to install packs into
  root: PathBuf,
  packy_url: Url,
  packs: CcPacks,
}

impl Default for Installer {
  fn default() -> Installer {
    trace!("using default installer");
    Installer::new("/usr/local/",Url::parse(PACKY_URL).unwrap(),CcPacks::default())
  }
}

impl Installer {
  fn new<P:AsRef<Path>>(root:P,packy_url:Url,packs:CcPacks) -> Installer { 
    let root = root.as_ref().to_path_buf();
    Installer {root,packy_url,packs}
  }
}

fn main() -> Result<()> {
  let args = Cli::parse();
  // init logger
  Logger::try_with_str(log_level_str_from_cli(args.level))?.start()?;
  trace!("{:?}", args);
  println!("Welcome to The Compiler Company.");
  println!("Detecting System...");

  let installer = Installer::default();
  println!("Installing...");
  println!("root path: {:?}",installer.root);
  println!("packy url: {}",installer.packy_url);
  println!("packs: {}",installer.packs);
  for i in installer.packs.0.iter() {
    // dl("http://google.com", "dl")?;
    info!("downloading {}", installer.packy_url.join(i.slug())?);
  }
  Ok(())
}
