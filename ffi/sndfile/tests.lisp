;;; sndfile/tests.lisp --- SNDFILE tests

;;; Code:
(defpackage :sndfile/tests
  (:use :cl :std :rt :sndfile :sb-ext :sb-alien))

(in-package :sndfile/tests)

(defsuite :sndfile)
(in-suite :sndfile)

(load-sndfile)

(deftest sanity ()
  (is (stringp (sf-version-string))))

(deftest list-formats ()
  "List all available audio file formats.")
