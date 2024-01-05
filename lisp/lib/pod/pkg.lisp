;;; lib/pod/pkg.lisp --- Pod package defs

;; Pod is a Lisp system for interacting with container runtimes --
;; i.e. Podman/Libpod.

;; Podman supports a RESTful API which is described here:
;; https://docs.podman.io/en/latest/_static/api.html

;; The API is the best way to interact with containers in almost all
;; cases but there are a few utilities for initializing the podman
;; server or spawning one in a user process when supported.

;;; Code:
(defpackage :pod
  (:use :cl :std :cli :sb-bsd-sockets :net :dat/json)
  (:export
   :*podman-api-version* :*podman-exe* :*buildah-exe*
   :*podman-local-user-socket*
   :pod-error :podman-error :libpod-error
   :decode-podman-response :encode-podman-request
   :podman-request :podman-response
   :libpod-request :libpod-request-json
   :start-podman-service :libpod-client))
