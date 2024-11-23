;;; socket.lisp --- Network Sockets

;; A simple wrapper for INET-SOCKETs

;;; Commentary:

;; No real need to wrap LOCAL-SOCKET - we just use what is provided by
;; SB-BSD-SOCKETS.

;;; Code:
(in-package :net/core)

(defclass net-socket (inet-socket) ()
  (:documentation "Simple wrapper for INET-SOCKETs provided by CORE."))
(defclass net-server (net-socket) ())
(defclass net-client (net-socket) ())
