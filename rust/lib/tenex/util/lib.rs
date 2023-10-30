//! Tenex utilities
// re-exports
#[cfg(feature = "oauth2")]
pub use oauth2;
#[cfg(feature = "indicatif")]
pub use indicatif;
/// OS-specific browser command. supports Win/Mac/Linux
pub fn open_browser(url: &str) {
  if cfg!(target_os = "windows") {
    // https://stackoverflow.com/a/49115945
    std::process::Command::new("rundll32.exe")
      .args(&["url.dll,FileProtocolHandler", url])
      .status()
      .expect("failed to open file");
  } else if cfg!(target_os = "macos") || cfg!(target_os = "linux") {
    // https://dwheeler.com/essays/open-files-urls.html
    #[cfg(target_os = "macos")]
    let cmd = "open";
    #[cfg(target_os = "linux")]
    let cmd = "xdg-open";

    #[cfg(any(target_os = "macos", target_os = "linux"))]
    {
      std::process::Command::new(cmd)
        .arg(url)
        .status()
        .expect("failed to open URL");
    }
  } else {
    unimplemented!() // ignore others
  }
}
