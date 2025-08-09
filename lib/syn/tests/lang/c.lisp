;;; c.lisp --- C Lang Tests

;; 

;;; Code:
(in-package :syn/tests/lang)

(in-suite :syn)

(deftest c-src ()
  (istype 'cons (parse-file :c (asdf:system-relative-pathname :tree-sitter "alien.c"))))
