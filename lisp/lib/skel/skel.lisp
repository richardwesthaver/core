;;; skel.lisp --- Skel Top-level

;; Top-level commands for interacting with the SKEL system.

;;; Code:
(in-package :skel)

(defmacro with-skel (&body body)
  `(progn
     (setf-skel-vars)
     ,@body))
