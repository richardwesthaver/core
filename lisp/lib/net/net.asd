(defsystem :net
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on 
  (:sb-concurrency :sb-posix 
   :sb-bsd-sockets :cl-ppcre 
   :swank-client :drakma 
   :hunchentoot :std :log)
  :serial t
  :components ((:file "pkg")
               (:file "err")
               (:file "obj")
               (:file "udp")
               (:file "tcp")
               (:module "codec"
                :components ((:file "punycode")
                             (:file "dns")
                             (:file "tlv")
                             (:file "osc")))
               (:module "proto" ;; https://wayland.app/protocols/
                :components ((:file "crew")
                             (:file "dns"))))
  :in-order-to ((test-op (test-op :net/tests))))

(defsystem :net/tests
  :depends-on (:rt :net)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :net)))
                
               
