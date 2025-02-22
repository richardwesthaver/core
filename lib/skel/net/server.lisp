;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defclass sk-request (request) ())
(defclass sk-response (response) ())

(defclass sk-service (udp-service) ()
  (:default-initargs
   :port *skel-service-port*
   :request-class 'sk-request
   :response-class 'sk-response))
