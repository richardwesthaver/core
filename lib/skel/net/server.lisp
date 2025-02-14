;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defclass skel-service (net/srv/udp:udp-service) ()
  (:default-initargs
   :port *skel-service-port*
   :request-class 'skel-request
   :response-class 'skel-response))
