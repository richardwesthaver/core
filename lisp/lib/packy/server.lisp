;;; packy/server.lisp --- Packy Server

;; Packy Server API.

;;; Commentary:

;; The server is responsible for provisioning connected clients with a package
;; registry API.

;; The server is HTTP/S compatible and based on NET/SRV protocol.

;;; Code:

(in-package :packy/server)

(defclass packy-service (service) ())

(defstruct packy-server)
