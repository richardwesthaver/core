;;; lan-party.lisp --- Simulate a complex network of UDP nodes

;; 

;;; Code:
(in-package :std-user)
(defpkg :bench/lan-party
  (:use :cl :std :net/srv/udp :log :json :obj :srv))
(in-package :bench/lan-party)
;; (make-instance 'udp-server)
(defclass lan-node (udp-service) ())
