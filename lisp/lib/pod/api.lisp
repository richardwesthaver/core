;;; lib/pod/obj.lisp --- Pod Objects

;;

;;; Code:
(defstruct podman-server)

(defun start-podman-server (&key local))

(defstruct podman-request)

(defstruct podman-response)

(defclass podman-connection (connection) ())

(defclass podman-client ()
  ((connection :type podman-connection)))
