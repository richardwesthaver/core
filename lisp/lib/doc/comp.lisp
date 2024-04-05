;;; lib/doc/comp.lisp --- Documentation Compiler

;; easiest way to ensure consistency in our docs is to compile them :)

;;; Code:
(in-package :doc)

;; Q 2023-12-28: generic compiler functions? hmm..

(defun compile-symbol-documentation (sym &key path))

(defun compile-package-documentation (pkg &key path))

(defun compile-file-documentation (file &key path)
  (with-compilation-unit (:policy '(optimize))
    (sb-ext:restrict-compiler-policy 'debug 3)
    (sb-ext:restrict-compiler-policy 'safety 3)
    (load file :verbose t)))

(defun compile-system-documentation (sys &key path)
  (unless (typep sys 'asdf:system)
    (setf sys (asdf:find-system sys)))
  (let ((sys-desc (asdf:system-description sys)))
