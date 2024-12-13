(in-package :gui/wm/x11)

(defvar *x11-display* nil)

(defun init-x11 ()
  (setq *default-display* (xlib:open-default-display)))

(defun display-fonts (&optional display (pattern "*"))
  (xlib:list-fonts (or display *default-display*) pattern))

(defun display-extensions (&optional display (result-type 'list))
  (xlib:list-extensions (or display *default-display*) :result-type result-type))
