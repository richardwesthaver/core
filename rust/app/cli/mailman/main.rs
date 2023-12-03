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
  let client = Client::new()
    .credentials(("ellis", "2pldn"))
    //    .credentials(Credentials::bearer("
    // 3XBDZ39pSQfrthH14cIFilI9raBN65zC8UKxcL2rp1XotT16FiN8yJVaFkabKL1h"))
    .connect("https://mail.compiler.company:8080")
    .await
    .unwrap();
  println!(
    "{:?}",
    client
      .email_query(
        Filter::and([email::query::Filter::has_keyword("$draft")]).into(),
        [email::query::Comparator::from()].into()
      )
      .await
      .unwrap()
      .take_ids()
  );
  // Query mailboxes to obtain Inbox and Trash folder id
  let inbox_id = client
    .mailbox_query(
      mailbox::query::Filter::role(mailbox::Role::Inbox).into(),
      None::<Vec<_>>,
    )
    .await
    .unwrap()
    .take_ids()
    .pop()
    .unwrap();
  let trash_id = client
    .mailbox_query(
      mailbox::query::Filter::role(mailbox::Role::Trash).into(),
      None::<Vec<_>>,
    )
    .await
    .unwrap()
    .take_ids()
    .pop()
    .unwrap();

  println!("Inbox: {:?}", inbox_id);
  println!("Trash: {:?}", trash_id);
}
