;;; proto.lisp --- SK-NET Protocols

;; 

;;; Code:
(in-package :skel/net/core)

(defvar *default-skel-server-port* 8008)
(defvar *skel-client-port-range* '(24000 . 25000))
(defvar *skel-service-port* *default-skel-server-port*)
