;;; lib/pod/pkg.lisp --- Pod package defs

;; Pod is a Lisp system for interacting with container runtimes --
;; i.e. Podman/Libpod.

;; Podman supports a RESTful API which is described here:
;; https://docs.podman.io/en/latest/_static/api.html

;; The API is the best way to interact with containers in almost all
;; cases but there are a few utilities for initializing the podman
;; server or spawning one in a user process when supported.

;;; Code:
(defpkg :box
  (:use :cl :std :cli :net :dat/json :config :obj :cli/tools/virt)
  (:export
   :box-config
   :archiso-config
   :qemu-image-config
   :qemu-system-config))

;;; DEFSYS Providers
;; (defprovider :box (name))
