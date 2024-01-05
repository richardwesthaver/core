;;; lib/pod/obj/pod.lisp --- Pod objects

;; Pods are a fairly loose construct in Podman - in short, a group of
;; containers.

;;; Code:
(in-package :pod)

(defclass live-pod (id) 
  (cgroup-parent cgroup-path containers create-cgroup create-command create-infra created hostname
   infra-config infra-container-id labels name namespace num-containers shared-namespaces state))
