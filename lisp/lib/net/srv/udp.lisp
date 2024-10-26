;;; udp.lisp --- UDP Services

;; Simple UDP Services

;;; Commentary:

;; UDP services are much more free-form than HTTP services and so typically
;; demand more from the programmer during implementation to be useful.

;; This module intends to glue together the NET/UDP and various NET/CODEC
;; packages (such as TLV) to support rapid development of message-based
;; protocols.

;;; Code:
(in-package :net/srv/udp)
