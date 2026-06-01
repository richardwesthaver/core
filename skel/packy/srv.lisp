;;; packy/srv.lisp --- Packy Server

;; Packy Server API.

;;; Commentary:

;; The server is responsible for provisioning connected clients with a package
;; registry API.

;; The server is HTTP/S compatible and based on NET/SRV protocol.

;;; Code:

(in-package :skel/packy)

(defclass packy-service (service) ())

(defun ensure-packy-directories (&optional (path *packy-home*))
  (dolist (p '("dist/" "doc/" "report/" "vc/"))
    (ensure-directories-exist (merge-pathnames p path))))

(defun ensure-dist-target-directories (&optional (path *packy-home*) (targets *machine-targets*))
  (dolist (p (mapcar (lambda (x) (directory-path (machine-target x))) targets))
    (ensure-directories-exist (merge-pathnames p path))))

(defun init-packy-server (&optional (home *packy-home*) (targets *machine-targets*))
  (ensure-packy-directories home)
  (ensure-dist-target-directories home targets)
  (dolist (target targets *packy-target-table*)
    (setf (gethash target *packy-target-table*) (make-config :target :name target))))
