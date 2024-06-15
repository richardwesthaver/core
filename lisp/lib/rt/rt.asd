;;; std.asd --- standard library
(defsystem :rt
  :depends-on (:std :log :dat :sb-sprof)
  :components ((:file "pkg")
               (:file "bench")
               (:file "tracing")
               (:file "flamegraph")
               (:file "cover"))
  :in-order-to ((test-op (test-op "rt/tests"))))

(defsystem :rt/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :rt)))
