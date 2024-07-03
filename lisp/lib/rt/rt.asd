;;; std.asd --- standard library
(defsystem :rt
  :depends-on (:std :log :dat :sb-sprof)
  :components ((:file "pkg")
               (:file "bench" :depends-on ("pkg"))
               (:file "tracing" :depends-on ("pkg"))
               (:file "flamegraph" :depends-on ("pkg"))
               (:file "cover" :depends-on ("pkg")))
  :in-order-to ((test-op (test-op "rt/tests"))))

(defsystem :rt/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :rt)))
