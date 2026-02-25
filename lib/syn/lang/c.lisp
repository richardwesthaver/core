;;; c.lisp --- C Syntax

;; 

;;; Code:
(defpackage :syn/lang/c
  (:nicknames :syn/c)
  (:use :cl :std :syn/lang :parse/pratt :tree-sitter :syn/ts)
  (:export))

(in-package :syn/lang/c)
(load-tree-sitter-c)

;; (parse-file :c #p"/usr/include/rocksdb/c.h" :consume t)

