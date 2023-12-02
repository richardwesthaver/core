use std::{env, path::PathBuf};

fn bindgen_rocksdb() {
  let bindings = bindgen::Builder::default()
    .header("/usr/include/rocksdb/c.h")
    .derive_debug(false)
    .blocklist_type("max_align_t") // https://github.com/rust-lang-nursery/rust-bindgen/issues/550
    .ctypes_prefix("libc")
    .size_t_is_usize(true)
    .generate()
    .expect("unable to generate rocksdb bindings");

  let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
  bindings
    .write_to_file(out_path.join("bindings.rs"))
    .expect("unable to write rocksdb bindings");
}

fn main() {
  bindgen_rocksdb();

  let target = env::var("TARGET").unwrap();
  // according to https://github.com/alexcrichton/cc-rs/blob/master/src/lib.rs#L2189
  if target.contains("apple")
    || target.contains("freebsd")
    || target.contains("openbsd")
  {
    println!("cargo:rustc-link-lib=dylib=c++");
  } else if target.contains("linux") {
    println!("cargo:rustc-link-lib=dylib=stdc++");
  }
  // Allow dependent crates to locate the sources and output directory of
  // this crate. Notably, this allows a dependent crate to locate the RocksDB
  // sources and built archive artifacts provided by this crate.
  println!(
    "cargo:cargo_manifest_dir={}",
    env::var("CARGO_MANIFEST_DIR").unwrap()
  );
  println!("cargo:out_dir={}", env::var("OUT_DIR").unwrap());
}
