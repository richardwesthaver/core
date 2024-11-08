;;; log.asd --- logging system
(defsystem :log
  :depends-on (:std :io :obj)
  :serial t
  :components ((:file "pkg")
               (:file "condition")
               (:file "log")
               (:file "stream"))
  :in-order-to ((test-op (test-op "log/tests"))))

(defsystem :log/tests
  :depends-on (:rt :std :log :io)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :log)))
