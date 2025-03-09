;;; gui/wm/x11/stump/pkg.lisp --- StumpWM Packages

;; StumpWM is an X11-based tiling window manager build in Common Lisp on top
;; of CLX/XLIB. On most CPU-bound systems running an X Display Server this is
;; our default WM.

;;; Code:
(defpackage :gui/x11/stump
  (:use :cl :stumpwm)
  (:export
   #:*app-map*
   #:*edit-map*
   #:*toggle-map*
   #:*nav-map*
   #:*sudo-map*
   #:*user-map*
   #:*default-stumpwm-modules*
   #:*default-stumpwm-prefix-key*
   #:start-stumpwm))

(in-package :gui/x11/stump)

(cli:defmain start-stumpwm ()
  (let ((argv (cli:args)))
    (if (find "--generate-manual" argv :test #'string-equal)
        (stumpwm::generate-manual)
        (stumpwm))))
