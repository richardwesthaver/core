;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defclass sk-server (net:udp-server) ()
  (:default-initargs
   :id "skel-server"
   :port *skel-service-port*))

