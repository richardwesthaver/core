use std::{borrow::Cow, process::Command};

use platforms::*;

/// Generate the `cargo:` key output
pub fn generate_cargo_keys() {
  let output = Command::new("hg").args(&["identify", "-i"]).output();

  let commit = match output {
    Ok(o) if o.status.success() => {
      let sha = String::from_utf8_lossy(&o.stdout).trim().to_owned();
      Cow::from(sha)
    }
    Ok(o) => {
      println!(
        "cargo:warning=Mercurial command failed with status: {}",
        o.status
      );
      Cow::from("unknown")
    }
    Err(err) => {
      println!("cargo:warning=Failed to execute hg command: {}", err);
      Cow::from("unknown")
    }
  };

  println!(
    "cargo:rustc-env=DEMON_VERSION={}",
    get_version_short(&commit)
  )
}

pub fn get_platform() -> String {
  let env_dash = if TARGET_ENV.is_some() { "-" } else { "" };

  format!(
    "{}-{}{}{}",
    TARGET_ARCH.as_str(),
    TARGET_OS.as_str(),
    env_dash,
    TARGET_ENV.map(|x| x.as_str()).unwrap_or(""),
  )
}

pub fn get_version(impl_commit: &str) -> String {
  let commit_dash = if impl_commit.is_empty() { "" } else { "-" };

  format!(
    "{}{}{}-{}",
    std::env::var("CARGO_PKG_VERSION").unwrap_or_default(),
    commit_dash,
    impl_commit,
    get_platform(),
  )
}

// TODO [2021-09-01 Wed 02:39] - reimpl this function via flag in `get_version`
fn get_version_short(impl_commit: &str) -> String {
  let commit_dash = if impl_commit.is_empty() { "" } else { "-" };

  format!(
    "{}{}{}",
    std::env::var("CARGO_PKG_VERSION").unwrap_or_default(),
    commit_dash,
    impl_commit,
  )
}
