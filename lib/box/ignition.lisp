;;; ignition.lisp --- CoreOS Ignition

;; Ignition JSON config parsing

;;; Commentary:

;; Objects <-> SXP <-> JSON

;; I really have no interest in reading or writing YAML, so instead we'll read
;; and write lisp. This package provides the role of Butane.

;; ref: https://coreos.github.io/butane

;; currently based on the 3.6.0 (WIP) 

;; spec: https://coreos.github.io/ignition/configuration-v3_6_experimental

;;; Example:
#| json
{
  "ignition": { "version": "3.6.0-experimental" },
  "systemd": {
    "units": [{
      "name": "example.service",
      "enabled": true,
      "contents": "[Service]\nType=oneshot\nExecStart=/usr/bin/echo Hello World\n\n[Install]\nWantedBy=multi-user.target"
    }]
  }
}
|#
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

(defmethod make-config ((self (eql :ignition)) &rest args)
  (apply 'make-instance 'ignition-config args))

(defconfig ignition-config-options () 
  ((merge)
   (replace)))

(defconfig ignition-storage-config ()
  ((files)
   (directories)
   (links)
   (luks)
   (filesystems)
   (disks)))

(defconfig ignition-timeouts-config ()
  ((http-response-headers)
   (http-total)))

(defconfig ignition-security-config ()
  ((tls)))

(defconfig ignition-proxy-config ()
  ((http-proxy)
   (https-proxy)
   (no-proxy)))

(defstruct ignition-disk device wipe-table partitions)

(defstruct ignition-partition label number size start)

(defstruct ignition-raid devices level name)

(defstruct ignition-filesystem device path format wipe-filesystem label)

(defstruct ignition-file path mode contents)

(defstruct ignition-file-contents source verification)

(defconfig ignition-systemd-config () 
  ((units)))

(defstruct ignition-systemd-unit
  name
  (enabled nil :type boolean)
  (mask nil :type boolean)
  dropins
  contents)

(defstruct ignition-systemd-dropin name contents)

(defconfig ignition-passwd-config () 
  ((users)
   (groups)))

(defstruct ignition-user 
  name password-hash ssh-authorized-keys uid 
  gecos home-dir no-create-home primary-group
  groups no-user-group no-log-init shell
  should-exist system)

(defstruct ignition-group
  name gid password-hash should-exist system)

(defconfig ignition-kernel-arguments ()
  ((should-exist) (should-not-exist)))

(defmethods build-ast
  (((self ignition-config) &key) (unwrap-object self)))
