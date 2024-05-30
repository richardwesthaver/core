;;; obj/graph/read.lisp --- #G reader macro and readtable

;;

;;; Code:
(in-package :obj/graph)

(defun |#G-reader| (stream sub-char numarg)
  "Enable the Graph reader for the following form."
  (declare (ignore sub-char))
  (std/readtable::|#~-reader|)

(defreadtable :graph
  "Graph notation readtable"
  (:merge :std)
  (:dispatch-macro-char #\# #\G #'|#G-reader|))
