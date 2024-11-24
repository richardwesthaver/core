(defsystem :cry
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on 
  (:ironclad :sb-concurrency :sb-posix 
             :sb-bsd-sockets :cl-ppcre
             :std :log :obj :dat
             :blake3 :io)
  :components ((:file "pkg")
               (:file "condition" :depends-on ("pkg"))
               (:file "crc64" :depends-on ("condition"))
               (:file "b3" :depends-on ("condition"))
               (:file "password" :depends-on ("condition"))
               (:file "authinfo" :depends-on ("condition"))
               (:file "jwt" :depends-on ("condition"))
               (:file "hotp" :depends-on ("condition"))
               (:file "totp" :depends-on ("hotp")))
  :in-order-to ((test-op (test-op :cry/tests))))

(defsystem :cry/tests
  :depends-on (:rt :cry)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cry)))
