;;; lan-party.lisp --- Simulate a complex network of UDP nodes

;; 

;;; Code:
(in-package :std-user)
(defpkg :bench/lan-party
  (:use :cl :std :net/srv/udp :log :json :obj))
(in-package :bench/lan-party)
;; (make-instance 'udp-server)
(defvar *default-config* (make-config :udp))

(defclass lan-node (udp-service) ())
