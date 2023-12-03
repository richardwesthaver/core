use clap::{Parser, Subcommand};
use krypt::{keyutils::keytypes, ks, ss, KryptConfig};
use logger::{debug, info, trace, warn, Logger};
use obj::Objective;
use std::path::PathBuf;
use util::Result;
#[derive(Debug, Parser)]
#[command(name="krypt",author, version, about, long_about = None)]
struct Cli {
  /// Command to run
  #[command(subcommand)]
  cmd: Option<Cmd>,
  /// Set the default config file
  #[arg(short, long, env = "KRYPT_CONFIG_FILE")]
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
  /// check service providers and config
  Check {
    /// Check for key service provider
    #[arg(short, long)]
    key: bool,
    /// Check for secret service provider
    #[arg(short, long)]
    secret: bool,
    /// Print the current config
    #[arg(short, long)]
    config: bool,
  },
  /// Show Krypt info
  Show {
    /// What to show
    kind: Option<String>,
  },
  /// Query the Krypt
  Search {
    /// Item kinds to search for
    #[arg(short, long, default_value = "password")]
    kind: String,
    /// Input string
    query: String,
  },
}

fn ll_str(b: u8) -> &'static str {
  match b {
    0 => "info",
    1 => "debug",
    _ => "trace",
  }
}

fn check_config(cfg: KryptConfig) -> Result<()> {
  println!("{}", cfg.to_json_string()?);
  Ok(())
}

fn check_ks() -> Result<()> {
  let mut session = ks::default_ks()?;
  let tokens = session.add_keyring("token")?;
  let mut passwords = session.add_keyring("password")?;
  let permissions = session.description()?.perms;
  passwords
    .add_key::<keytypes::User, &str, &[u8]>("foobar", "hackme".as_bytes())?;
  let user = passwords
    .search_for_key::<keytypes::User, &str, &mut ks::Keyring>(
      "foobar",
      &mut session,
    )?;
  info!("{:?}", &session);
  info!("{:?}", permissions);
  info!("security context: {:?}", session.security()?);
  info!("token keyring: {:?}", tokens);
  info!("password keyring: {:?}", passwords);
  // info!("{:?}",bigkey);
  info!("user: {:?}", user);
  session.revoke()?;
  Ok(())
}

async fn check_ss() -> Result<()> {
  ss::connect_ss().await?;
  Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
  // parse args
  let args = Cli::parse();
  // init logger
  Logger::try_with_str(ll_str(args.level))?.start()?;
  trace!("{:?}", args);
  // load config
  let cfg = if let Some(path) = args.cfg {
    match KryptConfig::load_file(path.clone()) {
      // FIXME
      Ok(c) => c,
      Err(e) => {
        warn!("{path:?}: {e}, using default config");
        KryptConfig::default()
      }
    }
  } else {
    KryptConfig::default()
  };
  debug!("{:?}", cfg);
  // run cmd
  if let Some(cmd) = args.cmd {
    match cmd {
      Cmd::Check {
        key,
        secret,
        config,
      } => {
        // check keyctl
        if key {
          check_ks()?
        } else {
        };
        // check secret service
        if secret {
          check_ss().await?
        } else {
        };
        // print the current config
        if config {
          check_config(cfg.clone())?
        } else {
        }; // FIXME
           // check all
        if key || secret || config == false {
          check_ks()?;
          check_ss().await?;
          check_config(cfg)
        } else {
          unreachable!()
        }
      }
      Cmd::Show { kind } => {
        // show something
        if let Some(k) = kind {
          match k.as_str() {
            _ => Ok(()),
          }
        } else {
          // show user keyring, fail if not exist
          let ur = ks::Keyring::attach(ks::SpecialKeyring::User)?;
          let d = ur.description()?;
          debug!("{ur:?} {d:?}");
          let data = ks::dump_keys(ur)?;
          println!("{data:?}");
          Ok(())
        }
      }
      Cmd::Search { kind, query } => {
        debug!("query: {:?}", query);
        match kind.as_str() {
          "password" | "pw" | "pwd" | "p" | "pass" => Ok(()),
          "key" | "k" => Ok(()),
          _ => todo!(),
        }
      }
    }
  } else {
    Ok(())
  }
}
