;;; readtable.lisp --- Documentation Readtable

;; DOC Readtable which overloads the standard '#|' reader macro.

;;; Commentary:

;; 
;;; Code:
(in-package :doc)
(defun vertical-bar-doc-reader (stream sub num))

(defreadtable :doc
  (:merge :std)
  (:dispatch-macro-char #\# #\| #'vertical-bar-doc-reader))
