//! cfg::config::user
//!
//! User configuration primitives
use crate::object::{direction::RelativeDirection, location::Point};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::{AuthConfig, DisplayConfig, PackageConfig, ProjectConfig};

/// User configuration type
///
/// Used to configure system users for various platforms.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct UserConfig {
  pub shell: ShellConfig,
  pub geo: Option<Point>,
  pub displays: Option<Vec<DisplayConfig>>,
  pub packages: Vec<PackageConfig>,
  pub projects: Vec<ProjectConfig>,
  pub auth: Vec<AuthConfig>,
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct ShellConfig {
  pub env: HashMap<String, String>,
  pub cmds: HashMap<String, String>,
  pub shell: ShellType,
}

#[derive(Serialize, Deserialize, Debug, Hash, Default)]
pub enum ShellType {
  #[default]
  Bash,
  Zsh,
  Sh,
}

/// A session is a single collection of `pseudo terminals` under the
/// management of tmux. Each session has one or more windows linked to
/// it.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct TmuxSessionConfig {
  pub name: String,
  pub windows: Vec<TmuxWindowConfig>,
}

/// A window occupies the entire screen and may be split into
/// rectangular panes.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct TmuxWindowConfig {
  pub name: String,
  pub panes: Vec<TmuxPaneConfig>,
}

/// An isolated pseudo terminal (pty) inside a Window, inside a
/// Session.
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct TmuxPaneConfig {
  pub name: String,
  pub position: Option<RelativeDirection>,
  pub init: Option<String>,
}
