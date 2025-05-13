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
(defpackage :std/defsys
  (:use :cl :asdf)
  (:nicknames :sys)
  (:export :defsys
   :find-system*
   :defsystem*))

(in-package :sys)

(defmacro defsys (name &body body)
  `(defsystem ,name ,@body))
