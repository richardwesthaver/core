//! src/sys/btrfsutil/build.rs --- libbtrfsutil bindgen builder
fn main() {
  println!("cargo:rustc-link-search=/usr/include");
  println!("cargo:rustc-link-lib=btrfsutil");
  println!("cargo:rerun-if-changed=wrapper.h");
  let bindings = bindgen::Builder::default()
    .header("wrapper.h")
    .prepend_enum_name(false)
    .derive_default(true)
    .generate_comments(true)
//    .allowlist_type("btrfs.*")
//    .allowlist_var("BTRFS.*")
    .parse_callbacks(Box::new(bindgen::CargoCallbacks))
    .generate()
    .expect("Unable to generate bindings");
  bindings
    .write_to_file(format!("{}/{}", std::env::var("OUT_DIR").unwrap(), "bindings.rs"))
    .expect("Couldn't write bindings!");
}
