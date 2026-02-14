;;; var.lisp --- SK-NET Variables

;; 

;;; Code:
(in-package :skel/net/core)

(defvar *default-skel-service-port* 8008)
(defvar *skel-client-port-range* (cons 24000 25000))
(defvar *skel-service-port* *default-skel-service-port*)
