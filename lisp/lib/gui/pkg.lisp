;;; pkg.lisp --- GUI Packages

;; 

;;; Commentary:

;;;; Window Systems

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
(defpackage :gui/core
  (:use :cl :std :log)
  (:export
   :gui-error
   :gui-client-p :gui-server-p))

(defpackage :gui/ext
  (:use :cl :std :log :gui/core)
  (:export 
   :*gui-backend-list*
   :*gui-backend*
   :register-gui-backend
   :load-gui-backend
   :with-gui-handlers
   :gui-main
   :define-gui))

#+x11
(defpackage :gui/x11
  (:shadowing-import-from :std/type :array-index)
  (:use :cl :std :gui/core :xlib)
  (:export
   #:display-extensions
   #:display-fonts
   #:init-x11
   #:*x11-display*))

#+wl
(defpackage :gui/wl
  (:use :cl :std :gui/core :wayflan)
  (:export))

#+clim
(defpackage :gui/clim
  (:use :clim :clim-lisp)
  (:shadowing-import-from :std)
  (:shadowing-import-from :cl))

(uiop:define-package :gui
  (:use :cl :std :log)
  (:use-reexport :gui/core :gui/ext
   #+clim :gui/clim
   #+wl :gui/wl 
   #+x11 :gui/x11))

(in-package :gui)

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
