;;; dbus.lisp --- DBUS Protocol

;; DBUS Protocol Definitions

;;; Code:
(in-package :net/proto/dbus)

(defclass dbus-connection (connection) ()
  (:documentation "A connection from a client to a DBUS server."))
