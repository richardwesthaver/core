;;; btrfs.lisp --- BTRFS common-lisp API

;; This package contains FFI bindings to the BTRFS C libraries libbtrfs and
;; libbtrfsutil.

;;; Commentary:

;;; Code:
(defpackage btrfs
  (:use :cl :std :sb-alien)
  (:export
   :load-btrfs :load-btrfsutil
   :+btrfs-lib-version+
   :btrfs-util-error :btrfs-util-error*
   :btrfs-util-strerror
   :btrfs-util-fs-sync
   :btrfs-util-subvolume-is-valid
   :btrfs-util-subvolume-get-id
   :btrfs-util-subvolume-get-path
   :btrfs-util-subvolume-get-info
   :btrfs-util-subvolume-get-read-only
   :btrfs-util-qgroup-inherit
   :btrfs-util-qgroup-inherit-create
   :btrfs-util-subvolume-snapshot
   :btrfs-util-subvolume-info
   :btrfs-util-subvolume-iter-destroy
   :btrfs-util-subvolume-iter-create
   :btrfs-util-subvolume-iter-next
   :btrfs-util-subvolume-iterator-get-fd
   :btrfs-util-qgroup-inherit-destroy
   :btrfs-util-subvolume-iterator
   :btrfs-util-qgroup-inherit
   :btrfs-util-subvolume-iterator))

(in-package :btrfs)

(define-alien-loader btrfs "/usr/lib/")

(define-alien-loader btrfsutil "/usr/lib/")
