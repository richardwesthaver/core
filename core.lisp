;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(in-package :std-user)

(defpkg :core 
  (:use :cl)
  (:use-reexport :std :log :io :obj :net :cry :parse :dat :cl-user :sb-ext :sb-debug))

(in-package :core)
(pushnew :core *features*)

;; (defun exclamation-mark-reader (stream inchar)
;;   (declare (ignore inchar))
;;   (read stream))

;; (defun question-mark-reader (stream inchar)
;;   "Reader function for the #\? macro character in the :CORE readtable. Processes
;; a query from input STREAM."
;;   (declare (ignore inchar)))

(defreadtable :core
  (:merge :modern :std :shell)
  ;; (:macro-char #\? #'question-mark-reader)
  ;; (:macro-char #\! #'exclamation-mark-reader)
  )

(define-lisp-package :core)

;; (with-core-lisp
