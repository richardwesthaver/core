;;; std.asd --- standard library
(defsystem :rt
  :depends-on (:std :log :dat :sb-sprof)
  :components ((:file "pkg")
               (:file "var" :depends-on ("pkg"))
               (:file "condition" :depends-on ("pkg"))
               (:file "util" :depends-on ("condition" "var"))
               (:file "proto" :depends-on ("pkg"))
               (:file "obj" :depends-on ("proto" "util"))
               (:file "rt" :depends-on ("obj"))
               (:file "tracing" :depends-on ("rt"))
               (:file "flamegraph" :depends-on ("rt"))
               (:file "cover" :depends-on ("rt"))
               (:file "fuzz" :depends-on ("rt")))
  :in-order-to ((test-op (test-op "rt/tests"))))

(defsystem :rt/tests
  :depends-on (:rt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :rt)))
