;;; tests.lisp --- Gstreamer FFI Tests

;; 

;;; Code:
(defpackage :gstreamer/tests
  (:use :cl :std :rt :gstreamer))

(in-package :gstreamer/tests)
(defsuite :gstreamer)
(in-suite :gstreamer)

(deftest sanity ())
