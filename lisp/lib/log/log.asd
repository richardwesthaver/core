;;; log.asd --- logging system
(defsystem :log
  :depends-on (:std)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "log/tests"))))

(defsystem :log/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :log)))
