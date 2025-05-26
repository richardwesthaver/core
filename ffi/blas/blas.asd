;;; blas.asd --- BLAS Sytem Definitions
(defsystem :blas
  :depends-on (:std :log)
  :components ((:file "blas"))
  :in-order-to ((test-op (test-op "blas/tests"))))

(defsystem :blas/tests
  :depends-on (:rt :blas :io)
  :components ((:file "tests/blas"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :blas)))
