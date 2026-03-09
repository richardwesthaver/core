;;; var.lisp --- DSP Variables

;; 

;;; Code:
(in-package :dsp/core)
(defvar *media-directory* #p"/opt/stash/media/")
(defvar *default-media-probe* t
  "Default profile for PROBE-MEDIA-FILE.

T = probe all slot values.
NIL = probe path and mime-type only.")

(define-logical-pathname "MEDIA" *media-directory*
  ("MEDIA:MUSIC;**;*.*.*" (merge-pathnames "music/**/*.*" *media-directory*))
  ("MEDIA:AUD;**;*.*.*" (merge-pathnames "aud/**/*.*" *media-directory*))
  ("MEDIA:VID;**;*.*.*" (merge-pathnames "vid/**/*.*" *media-directory*))
  ("MEDIA:IMG;**;*.*.*" (merge-pathnames "img/**/*.*" *media-directory*))
  ("MEDIA:SCREENSHOT;**;*.*.*" (merge-pathnames "screenshot/**/*.*" *media-directory*)))
