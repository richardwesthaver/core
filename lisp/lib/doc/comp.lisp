;;; lib/doc/comp.lisp --- Documentation Compiler

;; easiest way to ensure consistency in our docs is to compile them :)

;;; Code:
(in-package :doc)

;; Q 2023-12-28: generic compiler functions? hmm..

(defun compile-file-documentation (file)
  (with-compilation-unit (:policy '(optimize))
    (sb-ext:restrict-compiler-policy 'debug 3)
    (sb-ext:restrict-compiler-policy 'safety 3)
    (load file :verbose t)))

(defun compile-system-documentation (sys))

(defun compile-symbol-documentation (sym))

(defun compile-package-documentation (pkg))
