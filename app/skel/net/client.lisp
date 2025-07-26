;;; client.lisp --- Skel Network Client

;; 

;;; Code:
(in-package :skel/net/client)

(defservice sk-client (udp-client sk-service) ())
