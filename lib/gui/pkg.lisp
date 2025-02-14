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
