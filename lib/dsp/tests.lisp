;;; dsp/tests.lisp --- DSP Tests

;;

;;; Code:
(defpackage :dsp/tests
  (:use :cl :std :rt :log :aud :gstreamer))
(in-package :dsp/tests)
(defsuite :dsp)
(in-suite :dsp)

(deftest sanity ())
