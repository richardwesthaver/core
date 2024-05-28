(defsystem :vc
  :depends-on (:std :cli :obj :net :log :parse)
  :components ((:file "pkg")
               (:file "util")
               (:file "err")
               (:file "proto")
               (:file "hg")
               (:file "git"))
  :in-order-to ((test-op (test-op :vc/tests))))

(defsystem :vc/tests
  :depends-on (:rt :vc)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :vc)))
