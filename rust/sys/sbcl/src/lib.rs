//! lib.rs --- sbcl sys
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::ffi::c_char;

#[cfg(test)]
mod tests {
  use super::*;
  use libloading::{Library,Symbol};
  #[test]
  fn lisp_version_test() {
    unsafe {
      let lib = Library::new("/usr/local/lib/libsbcl.so").unwrap();
      let initialize_lisp = lib.get::<Symbol<extern "C" fn() -> std::ffi::c_int>>(b"initialize_lisp")
        .unwrap();
      initialize_lisp();
      // assert_eq!(res,0);
      //let lisp_version = lib.get::<Symbol<extern "C" fn()->&'static str>>(b"lisp_version")
      //.unwrap();
      //assert_eq!(lisp_version(), "2.4.2+");
    }
  }
}
