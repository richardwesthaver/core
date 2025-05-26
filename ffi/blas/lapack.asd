;;; lapack.asd --- LAPACK Sytem Definitions
(defsystem :lapack
  :depends-on (:std :log)
  :components ((:file "lapack")
               (:file "lapack-ffi"))
  :in-order-to ((test-op (test-op "lapack/tests"))))

(defsystem :lapack/tests
  :depends-on (:blas/tests)
  :components ((:file "tests/lapack"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :lapack)))
