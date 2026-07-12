;;; pkg.lisp --- SYN/GEN Tests

;; 

;;; Code:
(defpackage :syn/tests/gen
  (:use :cl :std :rt :syn/gen :syn/ts)
  (:export
   #:with-gen-test
   #:%in
   #:%out
   #:%bin
   #:*gen-test-files*))

(in-package :syn/tests/gen)

(defvar *gen-test-files* (system-relative-pathname :syn "tests/gen/"))

(defmacro with-gen-test (file &body body)
  `(let ((%in ,file)
         (%out (make-pathname :defaults ,file :type (string-downcase *gen*)))
         (%bin (make-pathname :defaults ,file :type "bin")))
     ,@body
     (unload-gen :c)))
