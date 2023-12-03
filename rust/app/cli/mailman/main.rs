use clap::{Parser, Subcommand};
use jmap_client::{client::Client, core::query::Filter, email, mailbox};
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
  /// Optional name to operate on
  name: Option<String>,

  /// Sets a custom config file
  #[arg(short, long, value_name = "FILE")]
  config: Option<PathBuf>,

  /// Turn debugging information on
  #[arg(short, long, action = clap::ArgAction::Count)]
  debug: u8,

  #[command(subcommand)]
  command: Option<Cmd>,
}

#[derive(Subcommand)]
enum Cmd {
  /// does testing things
  Test {
    /// lists test values
    #[arg(short, long)]
    list: bool,
  },
}

#[tokio::main]
async fn main() {
  let args = Cli::parse();
  println!("{:?}", args.name);
}
