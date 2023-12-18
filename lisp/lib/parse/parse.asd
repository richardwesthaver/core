(defsystem :parse
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/packy/issues"
  :depends-on (:cl-ppcre :std)
  :components ((:file "pkg")
               (:file "lex")
               (:file "lalr"))
  :in-order-to ((test-op (test-op :parse/tests))))

(defsystem :parse/tests
  :depends-on (:rt :parse)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :parse)))
