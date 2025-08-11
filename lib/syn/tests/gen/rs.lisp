;;; rs.lisp --- SYN/GEN/RS Tests

;; 

;;; Code:
(in-package :syn/tests/gen)
(defsuite :syn/gen/rs)
(in-suite :syn/gen/rs)

(deftest simple ()
  "Test a set of simple GEN/RS forms."
  (with-codegen :rs
    (with-gen-test (make-pathname :name "simple" :directory (namestring *gen-test-files*)
                                  :type (format nil "~A.lisp" *gen*))
      (gen-rs %in %out)
      (is (probe-file %out))
      ;; (run-cargo "build")
      ;; (is (probe-file %bin))
      ;; (delete-file %bin)
      ;;  TODO 2024-12-19: (run-cargo (namestring %out)) ;; use cargo-script
      ;; (is (probe-file %bin))
      ;; (isequal
      ;;  (format nil "Hello, World!~%")
      ;;  (with-output-to-string (s)
      ;;    (sb-ext:run-program %bin nil :output s :wait t)))
      )))
