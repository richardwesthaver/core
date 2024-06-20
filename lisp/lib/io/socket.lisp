;;; io/socket.lisp --- IO Sockets

;; 

;;; Code:
(in-package :io/socket)

(sb-bsd-sockets::define-socket-option-int
    sockopt-receive-timeout sockint::sol-socket sockint::so-rcvtimeo)
