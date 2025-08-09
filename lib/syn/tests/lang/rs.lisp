;;; rs.lisp --- RS Lang Tests

;; 

;;; Code:
(in-package :syn/tests/lang)
(in-suite :syn)
(in-readtable :std)
(defvar *rs-src* #"fn main() {
  util::bs::version::generate_cargo_keys();
}"#)

(deftest rust-src ()
  (istype 'sb-alien::alien-value (parse-string :rust *rs-src*)))
          
