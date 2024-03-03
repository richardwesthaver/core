;;; lib/gui/wm.lisp --- Window Management Systems

;;; Commentary:

;; The two Window Systems worth mentiong are X11 and Wayland. We are
;; interested in both, for different reasons.

;; X11 is for general-purpose, personal computing. It's the standard
;; everyone relies on and whenever we're running GUIs we're assuming
;; it's under X.

;; Wayland is for domain-specific user applications. It's a modular
;; protocol which follows a different design and philosophy than
;; X11. It is not feature complete, but it has a high level of
;; community support. In the right conditions, Wayland apps can be
;; smaller and faster than the equivalent X11-based implementation
;; [uncited]. e.g. kiosks.

;; No matter what, we have no intention of running X11 and Wayland in
;; parallel/embedded using things like XWayland. RTFM - it should be
;; clear which window management system an app is built for. If not,
;; it's a bug (squash it!).

;;; Code:
(in-package :gui/wm)

(defpackage :gui/wm/wl
  (:use :cl :std :gui/core :wayflan)
  (:nicknames :wl)
  (:export))

(defpackage :gui/wm/x11
  (:nicknames :x11)
  (:use :cl :std :gui/core :xlib)
  (:export))

(defconstant *default-wm* :x11)

(defun wm-package (&optional wm)
  "Return the WM package, either ':x11' for X11 or ':wl' for
Wayland. When no WM is provided, we interrogate the host to find out
which WM is currently running, and as a last resort falls back to
*DEFAULT-WM*."
  (case wm
    ((or :x11 :wl)  (find-package wm))
    ((nil) (find-package *default-wm*))
    (t (error "invalid wm type"))))
