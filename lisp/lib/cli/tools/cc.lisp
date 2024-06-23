;;; cli/tools/cc.lisp --- C Compilers

;; Use C Compiler tooling from Lisp.

;;; Commentary:

;; 

;;; Code:
(in-package :cli/tools/cc)

(defparameter *cc* (find-exe "clang"))

(defun run-cc (&rest args)
  (apply #'sb-ext:run-program *cc* (or args (list nil))))
