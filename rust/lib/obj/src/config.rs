//! cfg::config
//!
//! Primitive configuration types
pub mod auth;
pub mod database;
pub mod display;
pub mod library;
pub mod meta;
pub mod network;
pub mod package;
pub mod program;
pub mod project;
pub mod registry;
pub mod repo;
pub mod user;

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_package_cfg() {
    let mut pkg: package::PackageConfig = ron::from_str(
      r#"(name: "test-pack-cfg",
        repo: None,
        program: None,
        library: None)"#,
    )
    .unwrap();
    assert_eq!(pkg, package::PackageConfig::new("test-pack-cfg"));
    pkg.repo = Some(repo::RepoConfig::new());
    assert_eq!(repo::RepoConfig::new(), pkg.repo.unwrap());
  }

  #[test]
  fn test_network_cfg() {
    let mut net: network::NetworkConfig = ron::from_str(
      r#"(socket: "127.0.0.1:0",
        transport: "udp",
        tunnel: None,
        engine: Raw,
        peers:  None)"#,
    )
    .unwrap();
    assert_eq!(net, network::NetworkConfig::default());
    net.socket = "0.0.0.0:0".parse().unwrap();
    assert_eq!(net.socket, "0.0.0.0:0".parse().unwrap());
  }

  #[test]
  fn test_repo_config() {
    assert_eq!(repo::RepoConfig::default().vcs, "hg");
  }

  #[cfg(feature = "hg")]
  #[test]
  fn test_hgweb_insert() {
    let mut web_conf = repo::hg::HgwebConfig::default();

    web_conf
      .paths
      .insert(PathBuf::from("foo"), PathBuf::from("bar"));

    let wc2 = web_conf.paths.try_insert(
      PathBuf::from("contrib/lib/rust/tempdir"),
      PathBuf::from("contrib/lib/rust/tempdir"),
    );

    assert!(wc2.is_ok());
  }
}
