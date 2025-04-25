;;; lib/pod/podman.lisp --- Pod Manager

;;

;;; Code:
(in-package :pod)

(defvar *podman-config-directory* (merge-homedir-pathnames ".config/containers/"))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun start-podman-service (addr &optional (protocol :unix) (time 0))
  "Start the Libpod API on ADDR over PROTO which is either :TCP or :UNIX."
  (declare ((member :unix :tcp) protocol))
  (run-podman "system"
              "service"
              (format nil "~(~a~)://~a" protocol addr)
              (format nil "--time=~a" time)))
