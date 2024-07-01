(defsystem :cry
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on 
  (:ironclad :sb-concurrency :sb-posix 
             :sb-bsd-sockets :cl-ppcre
             :std :log :obj)
  :components ((:file "pkg")
               (:file "err" :depends-on ("pkg"))
               (:file "crc64" :depends-on ("pkg"))
               (:file "authinfo" :depends-on ("err"))
               (:file "jwt" :depends-on ("err"))
               (:file "hotp" :depends-on ("err"))
               (:file "totp" :depends-on ("hotp")))
  :in-order-to ((test-op (test-op :cry/tests))))

(defsystem :cry/tests
  :depends-on (:rt :cry)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cry)))
