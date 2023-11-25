;;; virt.lisp --- skel virtualization module

;; This package deals with 'virtualization' technologies used in our
;; development workflow.

;;; Commentary:

;; Virtualization tech includes but is not limited to QEMU, KVM,
;; Libvirt, Podman/Buddah, and systemd-nspawn. Our priority is a clean
;; integration with Podman which allows us to spin up and shutdown
;; containers according to the configuration found in skelfiles.

;; So, we need to investigate low-level podman interfaces - wire
;; protocol or FFI is preferred, which allows us to interact with the
;; podman daemon from a lisp process.

;; The other big component is Containerfile parser/compiler. A
;; Containerfile uses the same syntax as a Dockerfile internally, but
;; since we're hard-pivoting away from Docker ecosystem, we refer to
;; them using the 'Container' terminology.

;; The Containerfile format is quite simple, sharing similarities do
;; Makefiles and transitively Skelfiles. To build the virtual
;; Containerfile representation on the lisp side we can use Skel
;; classes and structures.

;; Similarly, there is a '.containerignore' (.dockerignore) file which
;; is used by container engines to modify the context to exclude
;; patterns. This is not a priority and it's very likely I will
;; account for this in a separate 'ignore.lisp' module.

;; Finally, there are some '.conf' files: 'containers.conf' and
;; 'container-mounts.conf' which will be implemented when I need them.

;; Resources:

;; https://github.com/containers/common/blob/main/docs/Containerfile.5.md

;; https://www.mankier.com/5/Containerfile

;; https://github.com/containers/buildah/discussions/3170

;; https://docs.docker.com/engine/reference/builder/

;; https://github.com/containers/common/blob/main/docs/containerignore.5.md

;; https://github.com/containers/common/blob/main/docs/containers-mounts.conf.5.md

;; https://github.com/containers/common/blob/main/docs/containers.conf.5.md

;;; Code:
(defpackage :skel/core/virt
  (:use :cl :skel/core/obj :skel/core/proto)
  (:export :containerfile))

(in-package :skel/core/virt)

(defparameter *default-containerfile* "containerfile")

(defclass containerfile (sk-meta)
  ((instructions :type (vector sk-command) :accessor cf-instructions)
  (directives :type (vector sk-command) :accessor cf-directives))
  (:documentation "A virtual Containerfile."))
