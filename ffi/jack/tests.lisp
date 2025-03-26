;;; tests.lisp --- JACK FFI Tests

;; 

;;; Code:
(defpackage :jack/tests
  (:use :cl :std :log :rt))
(in-package :jack/tests)
(defsuite :jack)
(in-suite :jack)
(deftest sanity () (iseql (schar (jack::jack-get-version-string) 0) #\1))
  
