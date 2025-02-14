;;; rs.lisp --- SYN/GEN/RS Tests

;; 

;;; Code:
(defpackage :syn/tests/gen/rs
  (:use :cl :syn/tests/gen :syn/gen :gen/rs)
  (:import-from :rt :deftest :is :iseql :isequal :in-suite :defsuite)
  (:import-from :cli/tools/rust :run-cargo)
  (:export
   #:*gen-rs-test-files*))

(in-package :syn/tests/gen/rs)
(defsuite :syn/gen/rs)
(in-suite :syn/gen/rs)

(defvar *gen-rs-test-files* (asdf:system-relative-pathname :syn "tests/gen/rs/"))

(deftest simple ()
  "Test a set of simple GEN/RS forms."
  (with-codegen :rs
    (with-gen-test (merge-pathnames "simple.sxp" *gen-rs-test-files*)
      (gen-rs %in %out)
      (is (probe-file %out))
      (delete-file %out)
      ;; (is (probe-file %bin))
      ;; (delete-file %bin)
      ;;  TODO 2024-12-19: (run-cargo (namestring %out)) ;; use cargo-script
      ;; (is (probe-file %bin))
      ;; (isequal
      ;;  (format nil "Hello, World!~%")
      ;;  (with-output-to-string (s)
      ;;    (sb-ext:run-program %bin nil :output s :wait t)))
      )))
