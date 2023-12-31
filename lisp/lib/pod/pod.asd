(defsystem :pod
  :depends-on (:std :cli :obj :dat :net :flexi-streams :dexador)
  :components ((:file "pkg")
               (:file "err")
               (:file "api")
               (:file "util")
               (:file "obj")
               (:file "containerfile"))
  :in-order-to ((test-op (test-op :pod/tests))))

(defsystem :pod/tests
  :depends-on (:rt :pod)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :pod)))
