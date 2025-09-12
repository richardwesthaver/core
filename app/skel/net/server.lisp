;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defservice sk-server (sk-service udp-service) ()
  (:default-initargs
   :port *skel-service-port*))
