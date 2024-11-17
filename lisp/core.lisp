;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(in-package :std-user)

(defpkg :core 
  (:use :cl :sb-ext)
  (:use-reexport :std :log :io :obj :net :cry :parse :syn :dat))

(define-lisp-package :core)
