;;; ignition.lisp --- CoreOS Ignition

;; Ignition JSON config parsing

;;; Commentary:

;; Objects <-> SXP <-> JSON

;; I really have no interest in reading or writing YAML, so instead we'll read
;; and write lisp. This package provides the role of Butane.

;; ref: https://coreos.github.io/butane

;; currently based on the 3.6.0 (WIP) 

;; spec: https://coreos.github.io/ignition/configuration-v3_6_experimental

;;; Code:
(in-package :box/ignition)

(define-constant +ignition-version+ "3.6.0-experimental" :test 'equal)
(defvar *ignition-config*)

(defconfig ignition-config (box-config) 
  ((version :initform +ignition-version+)
   config
   timeouts
   security
   proxy
   storage
   systemd
   passwd
   kernel-arguments))

(defconfig ignition-config-options (box-config) ())

(defconfig ignition-storage-config (box-config)
  ())

(defconfig ignition-systemd-config (box-config) ())

(defconfig ignition-passwd-config (box-config) ())
