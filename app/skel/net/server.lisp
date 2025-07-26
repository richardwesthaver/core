;;; server.lisp --- Skel Network Server

;; 

;;; Code:
(in-package :skel/net/server)

(defservice sk-server (sk-service udp-service) ()
  (:default-initargs
   :port *skel-service-port*))

(defmethod make-service ((self (eql :sk-server)) &rest args)
  (apply 'make-instance 'sk-server args))
