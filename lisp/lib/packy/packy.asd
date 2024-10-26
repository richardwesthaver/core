;;; packy.asd --- universal package manager
(defsystem "packy"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :std :cl-ppcre :rdb :obj :net :cli :io)
  :components
  ((:file "pkg")
   (:file "vars")
   (:file "proto")
   (:file "pkgbuild")
   (:file "db")
   (:file "client")
   (:file "server")
   (:file "packy"))
  :in-order-to ((test-op (test-op :packy/tests))))

(defsystem :packy/tests
  :depends-on (:rt :packy)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :packy)))
