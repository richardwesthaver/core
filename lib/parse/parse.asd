(require :sb-cltl2)
(defsystem :parse
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :depends-on (:cl-ppcre :std)
  :components ((:file "pkg")
               (:file "bytes")
               (:file "lex")
               (:file "pratt")
               (:file "yacc"))
  :in-order-to ((test-op (test-op :parse/tests))))

(defsystem :parse/tests
  :depends-on (:rt :parse)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :parse)))
