;;; std.asd --- standard library
(defsystem :rt
  :depends-on (:std :sxp :log)
  :components ((:file "pkg")
               (:file "bench")
               (:file "trace")
               (:file "flamegraph"))
  :in-order-to ((test-op (test-op "rt/tests"))))

(defsystem :rt/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :rt)))
