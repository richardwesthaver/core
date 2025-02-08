;;; cli.lisp --- Pod CLI

;; Container-tools Lisp CLI

;;; Code:
(in-package :pod)

(defcmd pod-info-cmd ()
  (unless (probe-file (podman-local-user-socket))
    (start-podman-service (podman-local-user-socket)))
  (with-libpod-client (c (make-instance 'pod:libpod-client))
    (log:info! "~A" (libpod-request c "_ping" :get))
    (inspect (libpod-request-json c "info"))))

(define-cli *pod-cli*
  :name "pod"
  :help t
  :description "container tools/libpod API client"
  :thunk pod-info-cmd)

(load-package-cli *pod-cli* :package :pod)
