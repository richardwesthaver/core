;;; tests.lisp --- JPEG FFI Tests

;; 

;;; Code:
(defpackage :jpeg/tests
  (:use :cl :std :sb-alien :rt :jpeg))
(in-package :jpeg/tests)
(defsuite :jpeg)
(in-suite :jpeg)
(load-jpeg)
(load-turbojpeg)
(deftest sanity () 
  (istype 'alien (jpeg::tj3init 0)))
