;;; c.lisp --- C Lang Tests

;; 

;;; Code:
(in-package :syn/tests/lang)
(defpackage :syn/tests/lang/c
  (:use :cl :syn/tests/lang :syn/lang/c :rt :syn/ts))

(in-package :syn/tests/lang/c)
(defsuite :syn/lang/c)
(in-suite :syn/lang/c)
(deftest c-src ()
  (istype 'cons (parse-file :c (asdf:system-relative-pathname :tree-sitter "alien.c"))))
