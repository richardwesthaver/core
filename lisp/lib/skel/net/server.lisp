;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defclass skel-request (request) ())
(defclass skel-response (response) ())

(defclass skel-service (net/srv/udp:udp-service skel-db) ()
  (:default-initargs
   :port *skel-port*
   :request-class 'skel-request
   :response-class 'skel-response))
