;;; packy/srv.lisp --- Packy Server

;; Packy Server API.

;;; Commentary:

;; The server is responsible for provisioning connected clients with a package
;; registry API.

;; The server is HTTP/S compatible and based on NET/SRV protocol.

;;; Code:

(in-package :skel/packy)

(defclass packy-service (service) ())

(defstruct packy-server)

(defun ensure-packy-directories (&optional (path *packy-home*))
  (dolist (p '("dist/" "data/" "doc/" "report/" "vc/"))
    (ensure-directories-exist (merge-pathnames p path)))
  (ensure-dist-target-directories path))

(defun ensure-dist-target-directories (&optional (path *packy-home*) (targets *packy-dist-targets*))
  (dolist (p (mapcar 'directory-path targets))
    (ensure-directories-exist (merge-pathnames p path))))
