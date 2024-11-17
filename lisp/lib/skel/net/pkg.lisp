;;; skel/net/pkg.lisp --- Skel Networking

;; SK-NET

;;; Code:
(defpackage :skel/net/core
  (:nicknames :sk-net-core)
  (:use :cl :log :std 
   :net/core :net/proto/dns :net/codec/tlv :skel/core/proto 
   :skel/core/obj :net/udp :net/tcp :obj/id 
   :skel/db
   :dat/proto :dat/sxp :dat/json)
  (:export
   #:*skel-client-port-range*
   #:*skel-service-port*))

(in-package :skel/net/core)

(defpackage :skel/net/client
  (:nicknames :sk-client)
  (:use :cl :std :net :sk-net-core)
  (:export))

(defpackage :skel/net/server
  (:nicknames :sk-server)
  (:use :cl :std :net/srv :sk-net-core :log)
  (:export))
