;;; pkg.lisp --- DSP Packages

;; 

;;; Code:
(defpackage :dsp/core
  (:use :cl :std :log)
  (:export))

(defpackage :dsp/gst
  (:use :cl :std :dsp/core :gstreamer :sb-alien)
  (:export))
