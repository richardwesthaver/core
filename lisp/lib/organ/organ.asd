(defsystem organ
  :version "0.1.0"
  :description "org-mode utils"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std/all :cl-ppcre :nlp)
  :in-order-to ((test-op (test-op :organ/tests)))
  :perform (test-op (o c) (symbol-call :rt :do-tests :organ))
  :build-operation "program-op"
  :build-pathname "organ"
  :entry-point "organ/cli:main")
