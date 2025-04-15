;;; dsp.lisp --- DSP Top-level

;; 

;;; Code:
(pkg:defpkg :dsp
  (:use :cl :std :log)
  (:use-reexport :dsp/core :dsp/av :dsp/aud :dsp/vid :dsp/gst))

(in-package :dsp)

(defun probe-media-file (path)
  "Probe PATH, checking that it is a supported media format and returning a subclass of MEDIA-FILE.")
