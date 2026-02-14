;;; c.lisp --- C Lang Tests

;; 

;;; Code:
(in-package :syn/tests/lang)

(in-suite :syn)

#+todo
(deftest c-src (:skip t)
  (istype 'cons (parse-file :c (system-relative-pathname :tree-sitter "alien.c"))))
