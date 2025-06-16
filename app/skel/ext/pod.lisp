;;; pod.lisp --- Skel POD extensions

;; Enable use of OCI Containerfiles and Podman ops.

;;; Code:
(in-package :skel/ext/pod)

(defclass sk-pod-project (sk-project)
  ((containers)))
