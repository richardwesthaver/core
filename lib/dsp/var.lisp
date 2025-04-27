;;; var.lisp --- DSP Variables

;; 

;;; Code:
(in-package :dsp/core)
(defvar *media-directory* #p"/opt/stash/media/")
(defvar *default-media-probe* t
  "Default profile for PROBE-MEDIA-FILE.

T = probe all slot values.
NIL = probe path and mime-type only.")

