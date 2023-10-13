//! src/sys/btrfs/lib.rs --- libbtrfs Rust bindings
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// NOTE 2023-08-20: the u128s aren't FFI-safe, but are just UUIDs on C-side so nbd
#![allow(improper_ctypes)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
