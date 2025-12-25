(defsystem :net
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :depends-on 
  (:dat :obj :io :parse
   :rt :std :cli :log :cry)
  :serial t
  :components 
  ((:file "pkg")
   (:file "obj")
   (:file "util")
   (:module "codec"
    :components ((:file "dns")
                 (:file "tlv")
                 (:file "osc")
                 (:file "http")
                 (:file "dbus")))
   (:file "udp")
   (:file "tcp")
   (:file "unix")
   (:module "proto"
    :components ((:file "dns")
                 (:file "swank")
                 (:file "crew")
                 (:file "http")
                 (:file "whois")
                 (:file "dict")
                 (:file "dbus")
                 (:file "ssh")
                 (:file "dm")
                 (:file "sesh")))
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
