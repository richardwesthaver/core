;;; tests.lisp --- Glib FFI Tests

;; 

;;; Code:
(defpackage :glib/tests
  (:use :cl :std :sb-alien :rt :glib))
(in-package :glib/tests)
(defsuite :glib)
(in-suite :glib)
(deftest sanity ())
