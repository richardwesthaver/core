;;; alsa/tests.lisp --- ALSA tests

;;; Code:
(defpackage :alsa/tests
  (:use :cl :std :rt :alsa :sb-ext :sb-alien))

(in-package :alsa/tests)

(defsuite :alsa)
(in-suite :alsa)

(load-alsa)

(deftest sanity ())
