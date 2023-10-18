//! cli module
pub use clap::{
  App, AppSettings, Arg, ArgGroup, ArgMatches, ArgSettings, ColorChoice, Subcommand, ValueHint,
};
#[cfg(feature = "bs")]
pub mod comp_gen {
  pub use clap_generate::{
    generate, generate_to,
    generators::{Bash, Elvish, Fish, PowerShell, Zsh},
    Generator,
  };
}
pub use indicatif;
pub use terminal_clipboard;
