;;; pkg.lisp --- SYN/GEN Tests

;; 

;;; Code:
(in-package :syn/tests)

(defpackage :syn/tests/gen
  (:use :cl :std :rt :syn/gen)
  (:export
   #:with-gen-test
   #:%in
   #:%out
   #:%bin))

(in-package :syn/tests/gen)

(defmacro with-gen-test (file &body body)
  `(let ((%in ,file)
         (%out (make-pathname :defaults ,file :type (string-downcase *gen*)))
         (%bin (make-pathname :defaults ,file :type "bin")))
     ,@body))
