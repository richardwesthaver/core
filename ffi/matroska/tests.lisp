;;; tests.lisp --- Matroska FFI Tests

;; 

;;; Code:
(defpackage :matroska/tests
  (:use :cl :std :sb-alien :rt :matroska))
(in-package :matroska/tests)
(defsuite :matroska)
(in-suite :matroska)
(load-matroska)
(deftest sanity ())
