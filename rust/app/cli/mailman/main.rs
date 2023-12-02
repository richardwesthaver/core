use jmap_client::{client::Client, core::query::Filter, email, mailbox};
#[tokio::main]
async fn main() {
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
