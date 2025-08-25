;;; readtable.lisp --- Q Readtable

;; 

;;; Code:
(in-package :q)
(defun q-reader (stream sub-char numarg))

(defreadtable :q
  (:merge :std)
  (:macro-char #\q #'q-reader))
