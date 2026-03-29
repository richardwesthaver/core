;;; sesh.lisp --- Ad-hoc Session Protocol

;; An extensible UDP-based session protocol.

;;; Code:
(in-package :net/proto/sesh)

(defclass sesh-router (router) ())

(defclass sesh-node (sesh-router peer) ())

(defclass sesh-server (sesh-node server) ())

(defclass sesh-client (sesh-node client) ())

(defclass sesh-connection (connection) ())
