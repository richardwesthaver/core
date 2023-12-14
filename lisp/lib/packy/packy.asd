;;; packy.asd --- universal package manager
(defsystem "packy"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/packy/issues"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :std :cl-ppcre :rdb)
  :components ((:file "pkg") (:file "proto") (:file "obj") (:file "db"))
  :in-order-to ((test-op (test-op :packy/tests))))

(defsystem :packy/tests
  :depends-on (:rt :packy)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :packy)))
