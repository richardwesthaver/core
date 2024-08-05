(defsystem :doc
  :version "0.1.0"
  :description "Core Documentation System"
  :depends-on (:std :organ :parse)
  :components ((:file "pkg")
               (:file "proto")
               (:file "err")
               (:file "symbol")
               (:file "file")
               (:file "package")
               (:file "system")
               (:file "dist")
               (:file "reprex")               
               (:file "image"))
  :in-order-to ((test-op (test-op :doc/tests))))

(defsystem :doc/tests
  :depends-on (:rt :doc :rdb)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :doc)))

