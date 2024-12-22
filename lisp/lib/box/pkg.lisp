;;; lib/pod/pkg.lisp --- Pod package defs

;; Pod is a Lisp system for interacting with container runtimes --
;; i.e. Podman/Libpod.

;; Podman supports a RESTful API which is described here:
;; https://docs.podman.io/en/latest/_static/api.html

;; The API is the best way to interact with containers in almost all
;; cases but there are a few utilities for initializing the podman
;; server or spawning one in a user process when supported.

;;; Code:
(defpackage :box
  (:use :cl :std :cli :sb-bsd-sockets :net :dat/json :config)
  (:export
   :*lxc-version*
   :box-config))

(defpackage :box/archiso
  (:nicknames :archiso)
  (:use :cl :std :cli/shell :dat/json :obj/config :box)
  (:export :*archiso-config* :*archiso-creds* :archiso-config))

(defpackage :box/qemu
  (:nicknames :qemu)
  (:use :cl :std :cli/shell :dat/json :config :box)
  (:export :*qemu-config* :*qemu-creds*))

(defpackage :box/qmp
  (:nicknames :qmp)
  (:use :cl :std :dat/json :net/srv :box/qemu)
  (:export :*archiso-config* :*archiso-creds*))
