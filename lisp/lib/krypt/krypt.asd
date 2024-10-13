(defsystem :krypt
  :version "0.1.0"
  :maintainer "Richard Westhaver <richard.westhaver@gmail.com>"
  :depends-on (:std :log :obj :dat :rdb :cry)
  :serial t
  :components ((:file "pkg")
               (:file "err")
               (:file "krypt"))
  :in-order-to ((test-op (test-op :krypt/tests))))

(defsystem :krypt/tests
  :depends-on (:rt :krypt)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :krypt)))
