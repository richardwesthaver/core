;;; cu.lisp --- gen/cu tests

;; 

;;; Code:
(in-package :syn/tests/gen)
(defsuite :syn/gen/cu)
(in-suite :syn/gen/cu)
(deftest simple ()
  "Test a set of simple GEN/CU forms."
  (with-codegen :cu
    (with-gen-test (make-pathname :name (format nil "simple.~A" *gen*) :type "lisp" :directory (namestring *gen-test-files*))
      (syn/gen/cu::gen-cu %in %out)
      (is (probe-file %out)))))
  
