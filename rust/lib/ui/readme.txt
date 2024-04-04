Slint UI by SixtyFPS is our preferred frontend for Rust.

This crate should focus exclusively on implementing callbacks/aligning
the machinery that is compiled from .slint files and exposing them to
higher-level crates.

Slint files will begin in Lisp and be compiled into a valid Slint UI
description as needed.

A neat feature of Slint is that it can compile UI definitions at
runtime. Something to keep in mind for user extensions. See the
'slint_interpreter' crate.

https://releases.slint.dev/1.5.1/docs/slint/ - Language Docs

https://releases.slint.dev/1.5.1/docs/rust/slint/ - Rust API Docs

https://slintpad.com/ - online playground
