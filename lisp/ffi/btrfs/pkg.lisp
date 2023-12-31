;;; src/fs/btrfs/btrfs.lisp --- BTRFS common-lisp API

;; This package contains FFI bindings to the BTRFS C libraries libbtrfs and
;; libbtrfsutil as well as some additional core routines from Rust.

;;; Commentary:

;; BTRFS is a core component of the NAS-T stack. We might even consider NAS-T as a
;; wrapper around BTRFS APIs in the same we we could say that TrueNAS is a wrapper
;; around ZFS.

;; NOTE 2023-09-03: currently the app has no concrete use-cases for accessing BTRFS APIs
;; directly from lisp. This will inevitably change, and we want the bindings for
;; debugging and experimentation.

;;; Code:
(defpackage btrfs/pkg
  (:use :cl :std :sb-alien)
  (:nicknames :btrfs)
  (:export
   :define-btrfs-ioctl))

(in-package :btrfs/pkg)

(define-alien-loader btrfs t)

(define-alien-loader btrfsutil t)

(defmacro define-btrfs-ioctl () "Define a wrapper for IOCTLs exposed by BTRFS.")
