;;; defsys.lisp --- defsystem extension macros

;; Intended to serve as a replacement for asdf:system utilities and quicklisp.

;;; Commentary:

;; goals:
;; - default to asdf (wrap)
;; - replace quicklisp (will need to be in lib/sys)
;; - share resources between system and dependency manager
;; - integrate with lib/packy (package distributor)
;; - multi-threaded by default
;; - parallel compilation (completely short-circuiting asdf)

;;; Code:
(in-package :std/defsys)
(defun module-provide-core (name)
  "Provide a CORE-MODULE, adding valid entries to the *CORE-MODULES*
  variable. The function USE should be called in order to load and activate a
  module, but the deprecated PROVIDE function is also supported."
  (or (module-provide-asdf name)
      (module-provide-contrib name)))

(defmacro defsys (name &body body)
  `(defsystem ,name ,@body))
