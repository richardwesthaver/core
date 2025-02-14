;;; gui/server.lisp --- GUI Server API

;; This package provides a server protocol for communication with GUI
;; Clients.

;; See GUI/CLIENT for details.

(in-package :gui/core)

(defgeneric gui-server-p (obj)
  (:method ((obj t)) nil))
