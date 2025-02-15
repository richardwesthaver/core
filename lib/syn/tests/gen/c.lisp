;;; c.lisp --- SYN/GEN/C Tests

;; 

;;; Code:
(in-package :syn/tests/gen)
(defpackage :syn/tests/gen/c
  (:use :cl :syn/tests/gen :syn/gen :gen/c)
  (:import-from :rt :deftest :is :iseql :isequal :in-suite :defsuite)
  (:import-from :cli/tools/cc :run-cc)
  (:export
   #:*gen-c-test-files*))

(in-package :syn/tests/gen/c)
(defsuite :syn/gen/c)
(in-suite :syn/gen/c)

(defvar *gen-c-test-files* (asdf:system-relative-pathname :syn "tests/gen/c/"))

(deftest simple ()
  "Test a set of simple GEN/C forms."
  (with-codegen :c
    (with-gen-test (merge-pathnames "simple.sxp" *gen-c-test-files*)
      (gen-c %in %out)
      (is (probe-file %out))
      (run-cc (namestring %out) (format nil "-o~A" %bin))
      (is (probe-file %bin))
      (isequal
       (format nil "Hello, World!~%")
       (with-output-to-string (s)
         (sb-ext:run-program %bin nil :output s :wait t)))
      ;; (delete-file %out)
      (std:println %out)
      (delete-file %bin))))


