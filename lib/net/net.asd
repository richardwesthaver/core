(defsystem :net
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :depends-on 
  (:dat :obj :io :parse
   :rt :std :cli :log :cry
   (:feature :swank :swank)
   (:feature :swank :swank-client)
   (:feature :swank :swank-crew))
  :serial t
  :components ((:file "pkg")
               (:file "obj")
               (:file "util")
               (:file "udp")
               (:file "tcp")
               (:module "codec"
                :components ((:file "dns")
                             (:file "tlv")
                             (:file "osc")
                             (:file "http")))
               (:module "proto"
                :components ((:file "swank" :if-feature :swank)
                             (:file "crew" :if-feature :swank)
                             (:file "http")
                             (:file "dns")
                             (:file "transmission")
                             (:file "whois")
                             (:file "ssh")
                             (:file "dm")
                             (:file "sesh")
                             (:file "nsm")))
               (:file "cookie")               
               (:file "req")
               (:module "srv"
                :components 
                ((:file "proto")
                 (:file "udp")
                 (:file "http")
                 (:file "oauth")
                 (:file "openapi")
                 (:file "ext")))
               (:file "net"))
  :in-order-to ((test-op (test-op :net/tests))))

(defsystem :net/tests
  :depends-on (:rt :net)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :net)))
