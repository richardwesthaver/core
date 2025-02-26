;;; rs.lisp --- RS Lang Tests

;; 

;;; Code:
(in-package :syn/tests/lang)
(defpackage :syn/tests/lang/rs
  (:use :cl :std :syn/tests/lang :syn/lang/rs :rt :syn/ts))
(in-package :syn/tests/lang/rs)
(defsuite :syn/lang/rs)
(in-suite :syn/lang/rs)
(deftest rust-src ()
  (istype 'cons
          (parse-file :rust (asdf:system-relative-pathname :core "crates/sxp/src/lib.rs"))))
