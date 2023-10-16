(defsystem :quiche
  :depends-on (:macs)
  :in-order-to ((test-op (test-op "quiche/tests")))
  :components ((:file "quiche/quiche")))

(defsystem :quche/tests
  :depends-on (:rt :quiche)
  :components ((:file "quche/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :quiche)))
