;; lib/pod/util.lisp --- Pod utilities

;;

;;; Code:
(in-package :pod)

(defun decode-podman-response (buf))

(defun encode-podman-request (obj))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun podman-run-command ())
