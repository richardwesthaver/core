;;; alsa/tests.lisp --- ALSA tests

;;; Code:
(defpackage :alsa/tests
  (:use :cl :std :rt :alsa :sb-ext :sb-alien))

(in-package :alsa/tests)

(defsuite :alsa)
(in-suite :alsa)

(load-asound)

(deftest sanity () 
  (iseql :double (alsa-element-type 'double-float))
  (iseql :float64-le (alsa-format-type 'double-float)))
