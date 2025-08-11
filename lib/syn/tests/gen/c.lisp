;;; c.lisp --- SYN/GEN/C Tests

;; 

;;; Code:
(in-package :syn/tests/gen)
(defsuite :syn/gen/c)
(in-suite :syn/gen/c)
(deftest simple ()
  "Test a set of simple GEN/C forms."
  (with-codegen :c
    (with-gen-test (make-pathname :name "simple" :type (format nil "~A.lisp" *gen*) :directory (namestring *gen-test-files*))
      (syn/gen/c::gen-c %in %out)
      (is (probe-file %out))
      (syn/gen/c::run-cc (namestring %out) (format nil "-o~A" %bin))
      (is (probe-file %bin))
      (isequal
       (format nil "Hello, World!~%")
       (with-output-to-string (s)
         (sb-ext:run-program %bin nil :output s :wait t)))
      ;; (delete-file %out)
      (std:println %out)
      (delete-file %bin))))
