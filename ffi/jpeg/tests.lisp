;;; tests.lisp --- JPEG FFI Tests

;; 

;;; Code:
(defpackage :jpeg/tests
  (:use :cl :std :sb-alien :rt :jpeg))
(in-package :jpeg/tests)
(defsuite :jpeg)
(in-suite :jpeg)
(jpeg::load-turbojpeg)
(deftest sanity () )
