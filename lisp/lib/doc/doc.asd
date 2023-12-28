(defsystem :doc
  :version "0.1.0"
  :description "Common Lisp Documentation System"
  :depends-on (:std :organ :parse)
  :components ((:file "pkg")
               (:file "err")
               (:file "symbol")
               (:file "package")
               (:file "system")
               (:file "file"))
  :in-order-to ((test-op (test-op :doc/tests))))

(defsystem :doc/tests
  :depends-on (:rt :doc)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :doc)))

