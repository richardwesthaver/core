;;; c.lisp --- SYN/GEN/C Tests

;; 

;;; Code:
(in-package :syn/tests/gen)
(defpackage :syn/tests/gen/c
  (:use :cl :syn/tests/gen :syn/gen :gen/c)
  (:import-from :rt :deftest :is :iseql))
(in-package :syn/tests/gen/c)

(deftest simple ()
  "Test a set of simple GEN/C forms."
  (c-reader)
  (in-package :syn/gen/c/sym)
  (read-gen-c-string "(function foo void ())"))
