;;; qemu.lisp --- QEMU

;; QEMU support for Common Lisp

;;; Commentary:

;; For now we merely want to be able to configure, build and launch images.

;;; Code:
(in-package :box/qemu)

(defconfig qemu-img-config (box-config) ())
