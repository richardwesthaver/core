;;; blas.asd --- BLAS Sytem Definitions
(defsystem :blas
  :depends-on (:std :log)
  :components ((:file "pkg")
               (:file "blas")
               (:file "lapack"))
  :in-order-to ((test-op (test-op "blas/tests"))))

(defsystem :blas/tests
  :depends-on (:rt :blas :io)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :blas)))
