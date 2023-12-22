;;; log.asd --- logging system
(defsystem :log
  :depends-on (:std)
  :serial t
  :components ((:file "pkg")
               (:file "err")
               (:file "log")
               (:file "source")
               (:file "sink"))
  :in-order-to ((test-op (test-op "log/tests"))))

(defsystem :log/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :log)))
