;;; skel/ext/net.lisp --- Skel Network

;; This extension adds a message-based network interface to the SKEL system
;; which can be initialized at runtime and used to communicate with remote
;; SKEL instances.

;;; Commentary:

;; The current design is meant to be as simple as possible. We currently only
;; support UDP socket communication over inet4 sockets.

;;;; Future Goals:

;; - unix sockets

;; - inet6

;; - wrapped streams (crypto)

;; - concurrency

;; - skelfile config

;; - protocol extensions

;;; Code:
(in-package :skel/ext/net)
