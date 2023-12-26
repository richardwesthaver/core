(defsystem :organ
  :version "0.1.0"
  :description "org-mode utils"
  :depends-on (:cl-ppcre :std :nlp :parse)
  :components ((:file "pkg")
               (:file "vars")
               (:file "proto")
               (:file "element")
               (:file "util"))
  :in-order-to ((test-op (test-op :organ/tests))))

(defsystem :organ/tests
  :depends-on (:rt :organ)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :organ)))
