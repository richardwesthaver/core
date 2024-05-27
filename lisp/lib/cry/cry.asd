(defsystem :cry
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on 
  (#+crypto :ironclad :sb-concurrency :sb-posix 
             :sb-bsd-sockets :cl-ppcre
             :std :log :obj)
  :serial t
  :components ((:file "pkg")
               (:file "err"))
  :in-order-to ((test-op (test-op :cry/tests))))

(defsystem :cry/tests
  :depends-on (:rt :cry)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cry)))
