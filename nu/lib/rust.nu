# rust.nu

export-env {
  $env.RUST = rustc
  $env.RUST_VERSION = null
  $env.CARGO = cargo
  $env.CARGO_VERSION = null
}

export def --env version [] {
  $env.RUST_VERSION = ((rustc --version) | split row ' ' | skip | first)
  $env.CARGO_VERSION = ((cargo --version) | split row ' ' | skip | first)
  print $env.RUST_VERSION
}

# export extern cargo []

# export extern rustc []
