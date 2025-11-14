;;; box/qmp.lisp --- QEMU Monitor Protocol

;; Lisp Interface for QMP commands, objects, and events.

;;; Commentary:

;; ref: https://www.qemu.org/docs/master/interop/qemu-qmp-ref.html
;; wiki: https://wiki.qemu.org/Documentation/QMP

;; QEMU is a complex, surprisingly flexible, and at times difficult piece of
;; software which is often used via proxy with VirtualBox or Proxmox - as in
;; via another application calling QEMU internally.

;; My problem with this is that too many of the implementation details are
;; hidden from the user, or covered up with additional abstractions. As the
;; developers maintain these abstractions they struggle to keep up with the
;; new low-level features implemented in their core systems.

;; To avoid the trickle-down economics of external application development, we
;; will develop a library which supports as much of the QMP and QAPI
;; surface-area that we deem suitable to support our applications.

;;; Code:
(in-package :box)
