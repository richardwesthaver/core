;;; var.lisp --- DSP Variables

;; 

;;; Code:
(in-package :dsp/core)
(eval-always (defvar *media-directory* #p"/opt/stash/media/"))
  
(defvar *default-media-probe* t
  "Default profile for PROBE-MEDIA-FILE.

T = probe all slot values.
NIL = probe path and mime-type only.")

(define-logical-host-loader "MEDIA" (&optional (home *media-directory*))
  `(("HOME;**;*.*.*" ,home)
    ("MUSIC;**;*.*.*" (merge-pathnames "music/**/*.*" ,home))
    ("AUD;**;*.*.*" (merge-pathnames "aud/**/*.*" ,home))
    ("VID;**;*.*.*" (merge-pathnames "vid/**/*.*" ,home))
    ("IMG;**;*.*.*" (merge-pathnames "img/**/*.*" ,home))
    ("SCREENSHOT;**;*.*.*" (merge-pathnames "screenshot/**/*.*" ,home))))
