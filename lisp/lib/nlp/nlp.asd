;;; nlp.asd --- Natural Language Processing
(defsystem "nlp"
  :version "0.1.0"
  :maintainer "Richard Westhaver <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :rdb :cl-ppcre :nlp/pkg)
  :in-order-to ((test-op (test-op :nlp/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :nlp)))
