;;; packy.asd --- universal package manager
(defsystem "packy"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/packy/issues"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :std :cl-ppcre :packy/pkg)
  :in-order-to ((test-op (test-op :packy/tests)))
  :perform (test-op (o c) (symbol-call :rt :do-tests :packy)))
