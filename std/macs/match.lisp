;;; match.lisp --- Simple Matching Macros

;; MATCH and EMATCH

;;; Code:
(in-package :std/macs)

(defmacro match (x &body body))
(defmacro ematch (x &body body))
