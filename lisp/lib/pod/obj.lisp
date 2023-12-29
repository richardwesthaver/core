;;; lib/pod/obj.lisp --- Pod Objects

;;

;;; Code:

(defun start-podman-service (&key local))

(defclass podman-client ()
  ((socket :type socket)))
