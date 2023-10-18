//! src/sys/btrfs/build.rs --- btrfs bindgen builder

//! Code:
fn main() {
  println!("cargo:rustc-link-search=/usr/include");
  println!("cargo:rerun-if-changed=wrapper.h");
  let bindings = bindgen::Builder::default()
    .header("wrapper.h")
    .generate_comments(true)
    .prepend_enum_name(false)
//    .use_core()
    .derive_default(true)
//    .allowlist_type("btrfs.*")
//    .allowlist_var("BTRFS.*")
    .parse_callbacks(Box::new(bindgen::CargoCallbacks))
    .generate()
    .expect("Unable to generate bindings");
  bindings
    .write_to_file(format!("{}/{}", std::env::var("OUT_DIR").unwrap(), "bindings.rs"))
    .expect("Couldn't write bindings!");
}
