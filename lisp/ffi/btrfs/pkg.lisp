;;; btrfs.lisp --- BTRFS common-lisp API

;; This package contains FFI bindings to the BTRFS C libraries libbtrfs and
;; libbtrfsutil.

;;; Commentary:

;;; Code:
(defpackage btrfs
  (:use :cl :std :sb-alien)
  (:export
   :load-btrfs :load-btrfsutil
   :btrfs-util-error :btrfs-util-error*
   :btrfs-util-strerror
   :btrfs-util-subvolume-iter-destroy
   :btrfs-util-subvolume-iterator-get-fd
   :btrfs-util-qgroup-inherit-destroy
   :btrfs-util-subvolume-iterator))

(in-package :btrfs)

(define-alien-loader btrfs "/usr/lib/")

(define-alien-loader btrfsutil "/usr/lib/")
