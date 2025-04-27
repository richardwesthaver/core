;;; dsp.lisp --- DSP Top-level

;; 

;;; Code:
(pkg:defpkg :dsp
  (:use :cl :std :log)
  (:use-reexport :dsp/core :dsp/av :dsp/aud :dsp/vid :dsp/gst))

(in-package :dsp)

(define-logical-pathname "MEDIA" #.*media-directory*
  ("MEDIA:MUSIC;**;*.*.*" (merge-pathnames "music/**/*.*" *media-directory*))
  ("MEDIA:AUD;**;*.*.*" (merge-pathnames "aud/**/*.*" *media-directory*))
  ("MEDIA:VID;**;*.*.*" (merge-pathnames "vid/**/*.*" *media-directory*))
  ("MEDIA:IMG;**;*.*.*" (merge-pathnames "img/**/*.*" *media-directory*)))

(defun probe-media-file (path)
  "Probe PATH, checking that it is a supported media format and returning a subclass of MEDIA-FILE.")
