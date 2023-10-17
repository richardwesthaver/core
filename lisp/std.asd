;;; std.asd --- standard library
(defsystem :std
  :pathname "std"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std/pkg :std/all)
  :in-order-to ((test-op (test-op "std/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))

(defsystem :std/tests
  :pathname "std"
  :depends-on (:std :rt)
  :components ((:file "tests")))

(asdf:register-system-packages "std/all" '(:std))
