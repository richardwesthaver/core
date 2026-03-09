;;; unix.lisp --- UNIX Domain Sockets

;; Support for local domain (AF_DOMAIN) sockets.

;;; Code:
(in-package :net/core)

(defconfig unix-socket-config (socket-config) 
  ())

(defclass unix-socket (socket) 
  ((family :initform sockint::af-local))
  (:default-initargs :type :stream))

(defmethod make-sockaddr-for ((socket unix-socket) &optional sockaddr &rest address)
  (apply '%sockaddr sockaddr address))
