;;; pkg.lisp --- low-level bindings to ALPM

;;; Commentary:

;;; Code:
(defpackage :alpm/pkg
  (:nicknames :alpm)
  (:use :cl :std)
  (:export 
   :load-alpm))

(in-package :alpm)

(defun load-alpm () 
  (unless (member :alpm *features*)
    (sb-alien:load-shared-object "libalpm.so" :dont-save t)
    (push :alpm *features*)))
