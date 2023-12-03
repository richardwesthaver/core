//! cli module
#[cfg(feature = "bs")]
pub use clap_complete;
pub use indicatif;
pub use terminal_clipboard;

pub fn log_level_str_from_cli(b: u8) -> &'static str {
  match b {
    0 => "info",
    1 => "debug",
    _ => "trace",
  }
}
