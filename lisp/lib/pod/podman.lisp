;;; lib/pod/podman.lisp --- Pod Manager

;;

;;; Code:
(in-package :pod)

(defvar *podman-exe* (find-exe "podman"))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun podman-run-command ())

(defun start-podman-service (addr &optional (protocol :unix) (time 0))
  "Start the Libpod API on ADDR over PROTO which is either :TCP or :UNIX."
  (declare ((member :unix :tcp) protocol))
  (sb-ext:run-program *podman-exe* `("system"
                                     "service"
                                     ,(format nil "~(~a~)://~a" protocol addr)
                                     ,(format nil "--time=~a" time))))
