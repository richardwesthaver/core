;;; cpp.lisp --- gen/cpp tests

;; 

;;; Code:
(in-package :syn/tests/gen)
(defsuite :syn/tests/gen/cpp)
(in-suite :syn/tests/gen/cpp)
(deftest simple ()
  "Test a set of simple GEN/CPP forms."
  (with-codegen :cpp
    (with-gen-test (make-pathname :name (format nil "simple.~A" *gen*) :type "lisp" :directory (namestring *gen-test-files*))
      (syn/gen/cpp::gen-cpp %in %out)
      (is (probe-file %out)))))

