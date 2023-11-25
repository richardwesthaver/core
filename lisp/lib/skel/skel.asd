(defsystem "skel"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/skel/issues"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :cl-ppcre :std :organ :skel/pkg)
  :in-order-to ((test-op (test-op skel/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests)))
