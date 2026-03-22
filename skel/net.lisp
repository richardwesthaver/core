;;; net.lisp --- Skel Networks

;; 

;;; Code:
(in-package :skel/net)

(defvar *default-skel-service-port* 8008)
(defvar *skel-client-port-range* (cons 24000 25000))
(defvar *skel-service-port* *default-skel-service-port*)

(defclass sk-server (udp-service) ()
  (:default-initargs
   :id "skel-server"
   :port *skel-service-port*))

(defservice sk-client (udp-client sk-service) ())
