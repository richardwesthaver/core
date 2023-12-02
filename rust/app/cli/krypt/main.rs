use util::Result;
use krypt::{ks,keyutils::keytypes};
#[tokio::main]
async fn main() -> Result<()> {
  // krypt::connect_ss().await.unwrap();
  let mut session = ks::default_ks()?;
  println!("{:?}",&session);
  let tokens = session.add_keyring("token")?;
  let mut passwords = session.add_keyring("password")?;
  let permissions = session.description()?.perms;
  println!("{:?}",permissions);
  println!("security context: {:?}",session.security()?);
  passwords.add_key::<keytypes::User,&str,&[u8]>("foobar", "hackme".as_bytes())?;
  let user = passwords.search_for_key::<keytypes::User,&str,&mut ks::Keyring>("foobar",&mut session)?;
  println!("token keyring: {:?}",tokens);
  println!("password keyring: {:?}",passwords);
  // println!("{:?}",bigkey);
  println!("user: {:?}",user);
  Ok(())
}
