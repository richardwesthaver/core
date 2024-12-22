;;; tests.lisp --- MPV FFI Tests

;; 

;;; Code:
(defpackage :mpv/tests
  (:use :cl :std :log :rt :mpv))

(in-package :mpv/tests)
(defsuite :mpv)
(in-suite :mpv)

(load-mpv)

(deftest sanity ()
  (istype 'integer (mpv-client-api-version)))
