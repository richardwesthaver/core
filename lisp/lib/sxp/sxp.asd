;;; sxp.asd --- S-eXPression data
(defsystem :sxp
  :depends-on (:std/named-readtables :std)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "sxp/tests"))))

(defsystem :sxp/tests
  :depends-on (:rt :sxp)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :sxp)))

(defsystem :sxp/bench
  :depends-on (:rt :sxp)
  :components ((:file "bench")))
