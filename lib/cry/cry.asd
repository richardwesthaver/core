(defsystem :cry
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on 
  (:ironclad :sb-concurrency 
   :sb-posix :sb-bsd-sockets 
   :std :log :obj :dat
   :blake3 :keyutils
   :openssl :io)
  :components ((:file "pkg")
               (:file "condition" :depends-on ("pkg"))
               (:file "crc64" :depends-on ("condition"))
               (:module "tls"
                :components 
                ((:file "pkg")
                 (:file "var")
                 (:file "bio")
                 (:file "condition")
                 (:file "funcall")
                 (:file "init")
                 (:file "stream")
                 (:file "x509")
                 (:file "ctx")
                 (:file "verify")))
               (:file "b3" :depends-on ("condition"))
               (:file "password" :depends-on ("condition"))
               (:file "authinfo" :depends-on ("condition"))
               (:file "keyring" :depends-on ("condition"))
               (:file "jwt" :depends-on ("condition"))
               (:file "hotp" :depends-on ("condition"))
               (:file "totp" :depends-on ("hotp"))
               (:file "drm" :depends-on ("condition"))
               (:file "gpg" :depends-on ("condition"))
               (:file "ssh" :depends-on ("condition"))
               (:file "sign" :depends-on ("condition"))
               (:file "auth" :depends-on ("password" "gpg" "jwt" "authinfo" "keyring" "hotp" "totp"))
               (:file "cry" :depends-on ("pkg")))
  :in-order-to ((test-op (test-op :cry/tests))))

(defsystem :cry/tests
  :depends-on (:rt :cry :net)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :cry)))
