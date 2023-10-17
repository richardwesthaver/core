(defsystem :quiche
  :depends-on (:std)
  :in-order-to ((test-op (test-op "quiche/tests")))
  :components ((:file "quiche")))

(defsystem :quiche/tests
  :depends-on (:rt :quiche)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :quiche)))
