(defpackage :gui/x11
  (:shadowing-import-from :std/type :array-index)
  (:use :cl :std :gui/core :xlib)
  (:export
   #:display-extensions
   #:display-fonts
   #:init-x11
   #:*x11-display*))

(in-package :gui/x11)

(defvar *x11-display* nil)

(defun init-x11 ()
  (setq *default-display* (xlib:open-default-display)))

(defun display-fonts (&optional display (pattern "*"))
  (xlib:list-fonts (or display *default-display*) pattern))

(defun display-extensions (&optional display (result-type 'list))
  (xlib:list-extensions (or display *default-display*) :result-type result-type))
