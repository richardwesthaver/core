;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defclass skel-request (request) ())
(defclass skel-response (response) ())

(defclass skel-service (service) ()
  (:default-initargs
   :port *skel-service-port*
   :request-class 'skel-request
   :response-class 'skel-response))
