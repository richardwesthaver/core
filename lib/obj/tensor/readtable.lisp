;;; readtable.lisp --- Tensor Readtable

;; Tensor-related readtable

;;; Commentary:

;; Tensor Indexing Notation

;;; Code:
(in-package :obj/tensor)

(defreadtable :tensor
  "The tensor readtable, enabling tensor indexing notation."
  (:merge :modern))
