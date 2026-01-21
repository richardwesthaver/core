;;; cli.lisp --- Pod CLI

;; Container-tools Lisp CLI

;;; Code:
(in-package :pod)

(defcommand (:pod info) ()
  (unless (probe-file (podman-local-user-socket))
    (start-podman-service (podman-local-user-socket)))
  (with-libpod-client (c (make-instance 'pod:libpod-client))
    (log:info! "~A" (libpod-request c "_ping" :get))
    (inspect (libpod-request-json c "info"))))

#+todo
(define-cli "pod" :description "container tools/libpod API client")
  

