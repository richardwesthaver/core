;;; readtable.lisp --- Documentation Readtable

;; DOC Readtable which overloads the standard '#|' reader macro
;; sb-impl::sharp-vertical-bar.

;;; Commentary:

;; 

;;; Code:
(in-package :doc)

(defun vertical-bar-doc-reader (stream sub num)
  (sb-impl::sharp-vertical-bar stream sub num)
  (values))

(defreadtable :doc
  (:merge :std)
  (:dispatch-macro-char #\# #\| #'vertical-bar-doc-reader))


