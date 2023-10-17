;;; std.asd --- standard library
(defsystem :std
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std/pkg :std/all)
  :in-order-to ((test-op (test-op "std/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :std)))

(register-system-packages "std/all" '(:std))
(register-system-packages "std/named-readtables" '(:named-readtables))
(register-system-packages "std/rt" '(:rt))
(register-system-packages "std/sxp" '(:sxp))
(register-system-packages "std/cli" '(:cli))
