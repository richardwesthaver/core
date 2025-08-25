;;; readtable.lisp --- Tensor Readtable

;; Tensor-related readtable

;;; Commentary:

;; Tensor Indexing Notation

;;; Code:
(in-package :obj/tensor)

;; TODO 2025-08-24: 
(defun t-reader (stream sub-char numarg))

(defreadtable :tensor
  "The tensor readtable, enabling tensor indexing notation."
  (:merge :modern)
  (:macro-char #\t #'t-reader))
