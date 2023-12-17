;;; std.asd --- standard library
(defsystem :rt
  :depends-on (:std :sxp :log)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "std/tests"))))

(defsystem :rt/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :rt)))