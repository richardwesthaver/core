;;; nlp.asd --- Natural Language Processing
(defsystem "nlp"
  :version "0.1.0"
  :maintainer "Richard Westhaver <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:std :rdb :ppcre :parse :obj)
  :components ((:file "pkg")
               (:file "data")
               (:file "tokenize")
               (:file "doc")
               (:module "stem"
                :components
                ((:file "porter")))
               (:file "textrank")
               (:file "dbscan")
               (:file "section")
               (:file "string")
               (:file "fuzzy"))
  :in-order-to ((test-op (test-op :nlp/tests))))


(defsystem :nlp/tests
  :depends-on (:rt :log :nlp)
  :components ((:file "tests"))
  :in-order-to ((test-op (uiop:symbol-call :rt :do-tests :nlp))))
