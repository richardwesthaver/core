;;; std.asd --- standard library
(defsystem :std
  :pathname "std"
  :class :package-inferred-system
  :depends-on (:std/all)
  :in-order-to ((test-op (test-op "std/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
