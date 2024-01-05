;;; lib/pod/podman.lisp --- Pod Manager

;;

;;; Code:
(in-package :pod)

(defvar *podman-exe* (find-exe "podman"))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun podman-run-command ())

(defun start-podman-service (addr &optional (time 0))
  "Start the Libpod API on ADDR which should be a valid uri beginning
with tcp:// or unix://."
  (sb-ext:run-program *podman-exe* `("system" "service" ,addr ,(format nil "--time=~a" time))))

