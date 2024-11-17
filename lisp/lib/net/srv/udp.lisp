;;; udp.lisp --- UDP Services

;; Simple UDP Services

;;; Commentary:

;; UDP services are stateless so typically demand more from the programmer
;; during implementation to be useful.

;; This module intends to glue together the NET/UDP and various NET/CODEC
;; packages (such as TLV) to support rapid development of message-based
;; protocols.

;;; Code:
(in-package :net/srv/udp)

(defclass udp-service (service) ())
(defclass udp-echo-service (udp-service) ())
