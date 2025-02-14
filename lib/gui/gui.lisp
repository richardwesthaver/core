;;; gui.lisp --- Top-level GUI

;; 

;;; Code:
(pkg:defpkg :gui
  (:use :cl :std :log)
  (:use-reexport :gui/core :gui/ext))

(in-package :gui)

(when (sb-int:featurep :x11)
  (use-package :gui/x11))
(when (sb-int:featurep :wl)
  (use-package :gui/wl))

(defvar *default-display-protocol* :gui/x11)

(defun display-protocol-package (&optional proto)
  "Return the WM package, either ':x11' for X11 or ':wl' for
Wayland. When no WM is provided, we interrogate the host to find out
which WM is currently running, and as a last resort falls back to
*DEFAULT-WM*."
  (case proto
    ((or :x11 :wl)  (find-package proto))
    (null (find-package *default-display-protocol*))
    (t (error "invalid wm type"))))
