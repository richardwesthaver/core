(push :ssl *features*)
(defsystem :net
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on 
  (:sb-concurrency :sb-posix 
   :sb-bsd-sockets :cl-ppcre
   :fast-io :swank
   :dat :obj
   :io :parse
   :swank-client
   :cl+ssl :cli
   :chipz :babel :chunga
   :std :log)
  :serial t
  :components ((:file "pkg")
               (:file "condition")
               (:file "obj")
               (:file "util")
               (:file "udp")
               (:file "tcp")
               (:module "codec"
                :components ((:file "punycode")
                             (:file "dns")
                             (:file "tlv")
                             (:file "osc")))
               (:module "proto"
                :components ((:file "crew")
                             (:file "swank")
                             (:file "http")
                             (:file "dns")
                             (:file "ssh")))
               (:file "cookie")
               (:file "req")
               (:file "fetch"))
  :in-order-to ((test-op (test-op :net/tests))))

(defsystem :net/tests
  :depends-on (:rt :net)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :net)))
                
               
