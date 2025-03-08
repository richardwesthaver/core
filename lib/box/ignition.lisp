;;; ignition.lisp --- CoreOS Ignition

;; 

;;; Code:
(in-package :box/ignition)

(defvar *ignition-config*)

(defconfig ignition-config (box-config) 
  ((version :initform "3.5.0")
   config
   timeouts
   security
   proxy
   storage
   systemd
   passwd
   kernel-arguments))
