;;; dsp/tests.lisp --- DSP Tests

;;

;;; Code:
(defpackage :dsp/tests
  (:use :cl :std :rt :log :dsp))

(in-package :dsp/tests)

(defsuite :dsp)
(in-suite :dsp)

(deftest sanity ()
  (is (load-gst))
  (is (load-av)))
