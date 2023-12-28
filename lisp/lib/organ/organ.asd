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
                ((:module "lesser"
                  :components ((:file "paragraph")
                               (:file "block")
                               (:file "clock")
                               (:file "diary-sexp")
                               (:file "latex-env")
                               (:file "comment")
                               (:file "fixed-width")
                               (:file "horizontal-rule")
                               (:file "keyword")
                               (:file "planning")
                               (:file "table-row")
                               (:file "node-property")))
                 (:module "greater"
                  :components ((:file "plain-list")
                               (:file "block")
                               (:file "drawer")
                               (:file "footnote-def")
                               (:file "item")
                               (:file "table")))
                 (:file "headline")))
               (:file "util")
               (:file "section")
               (:file "heading")
               (:file "document"))
  :in-order-to ((test-op (test-op :organ/tests))))

(defsystem :organ/tests
  :depends-on (:rt :organ)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :organ)))
