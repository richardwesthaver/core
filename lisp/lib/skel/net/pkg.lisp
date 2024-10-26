;;; skel/net/pkg.lisp --- Skel Networking

;; SK-NET

;;; Code:
(defpackage :skel/net
  (:nicknames :sk-net)
  (:use :cl :log :std :net/core :net/proto/dns :net/codec/tlv :skel/core :net/udp :net/tcp :obj/id :dat/proto :dat/sxp :dat/json)
  (:export
   #:*skel-client-port-range*
   #:*skel-service-port*))

(in-package :skel/net)

(defpackage :skel/net/client
  (:nicknames :sk-client)
  (:use :cl :std :net :skel/net)
  (:export))

(defpackage :skel/net/server
  (:nicknames :sk-server)
  (:use :cl :std :net/srv :skel/net :log)
  (:export))
