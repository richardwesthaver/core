;;; ext.lisp --- Manage External Network Services

;; Light wrappers for typical host web servers (Caddy/Nginx/etc)

;;; Commentary:

;; Nginx services are considered 'fully managed'. They are managed entirely
;; from outside Lisp and we only bother to control them via systemd with root
;; privileges.

;; Caddy services are sometimes fully managed, as when launched via systemd
;; using '/etc/Caddyfile' for configuration. However they may also be launched
;; and managed from Lisp with an admin endpoint (by default at localhost:2019)
;; which allows for dynamic control via REST API client.

;;; Code:
(in-package :net/srv/ext)

(defclass external-network-service () ())

(defclass caddy-service (external-network-service)
  ((process :initform nil :initarg :process :accessor process)
   (config)))

(defmethod start ((self caddy-service))
  (setf (process self) (sb-ext:run-program *caddy* '("run") :wait nil))
  self)

(defmethod stop ((self caddy-service) &key)
  (sb-ext:process-kill (process self) 0))

;; not that useful
(defclass nginx-service (external-network-service) ())
