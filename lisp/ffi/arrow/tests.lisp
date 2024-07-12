;;; tests.lisp --- Apache Arrow FFI Tests

;; 

;;; Code:
(defpackage :arrow/tests
  (:use :cl :std :sb-alien :rt :arrow))
(in-package :arrow/tests)
(defsuite :arrow)
(in-suite :arrow)
(load-arrow)
(deftest sanity ())
