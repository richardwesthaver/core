(defsystem :organ
  :version "0.1.0"
  :description "org-mode utils"
  :depends-on (:cl-ppcre :std :nlp :parse)
  :components ((:file "pkg")
               (:file "vars")
               (:file "err")
               (:file "proto")
               (:file "macs")
               (:module "object"
                :components
                ((:file "markup")
                 (:file "entity")
                 (:file "citation")
                 (:file "footnote-ref")
                 (:file "inline-babel-call")
                 (:file "inline-source-block")
                 (:file "line-break")
                 (:file "link")
                 (:file "macro")
                 (:file "export-snippet")
                 (:file "sscript")
                 (:file "stat-cookie")
                 (:file "table-cell")
                 (:file "target")
                 (:file "timestamp")))
               (:module "element"
                :components 
                ((:file "lesser/paragraph")
                 (:file "greater/plain-list")
                 (:file "headline")))
               (:file "section")
               (:file "heading")
               (:file "document")
               (:file "util"))
  :in-order-to ((test-op (test-op :organ/tests))))

(defsystem :organ/tests
  :depends-on (:rt :organ)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :organ)))
