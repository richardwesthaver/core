;;; tests.lisp --- Web Tests

;; 

;;; Code:
(defpackage :web/tests
  (:use :cl :std :rt :web/wasm/binary :web/wasm/text :web/sys))
(in-package :web/tests)
(defsuite :web)
(in-suite :web)
