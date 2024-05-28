;;; nlp.asd --- Natural Language Processing
(defsystem "nlp"
  :version "0.1.0"
  :maintainer "Richard Westhaver <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :rdb :cl-ppcre :parse :nlp/pkg)
  :components ((:file "pkg")
               (:file "data")
               (:file "tokenize")
               (:file "doc")
               (:module "stem"
                :components
                ((:file "porter")))
               (:file "textrank")
               (:file "dbscan")
               (:file "section"))
  :in-order-to ((test-op (test-op :nlp/tests))))


(defsystem :nlp/tests
  :depends-on (:nlp :std :rt)
  :components ((:file "tests"))
  :in-order-to ((test-op (rt:do-tests :nlp))))
