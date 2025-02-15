;;; tests.lisp --- JPEG FFI Tests

;; 

;;; Code:
(defpackage :jpeg/tests
  (:use :cl :std :sb-alien :rt :jpeg))
(in-package :jpeg/tests)
(defsuite :jpeg)
(in-suite :jpeg)
(jpeg::load-jpeg)
(deftest sanity () 
  (istype 'integer jpeg::+libjpeg-turbo-version-number+)
  (is> jpeg::+jpeg-lib-version+ 60))
