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

(define-alien-loader "btrfs" t "/usr/lib/")

(define-alien-loader "btrfsutil" t "/usr/lib/")

(defmacro define-btrfs-ioctl () "Define a wrapper for IOCTLs exposed by BTRFS.")

(define-alien-enum (btrfs-util-error int)
                   :ok 0
                   :stop-iteration 1
                   :no-memory 2
                   :invalid-argument 3
                   :not-btrfs 4
                   :not-subvolume 5
                   :subvolume-not-found 6
                   :error-open-failed 7
                   :error-rmdir-failed 8
                   :error-unlink-failed 9
                   :error-stat-failed 10
                   :error-statfs-failed 11
                   :error-search-failed 12
                   :error-ino-lookup-failed 13
                   :error-subvol-getflags-failed 14
                   :error-subvol-setflags-failed 15
                   :error-subvol-create-failed 16
                   :error-snap-create-failed 17
                   :error-snap-destroy-failed 18
                   :error-default-subvol-failed 19
                   :error-sync-failed 20
                   :error-start-sync-failed 21
                   :error-wait-sync-failed 22
                   :error-get-subvol-info-failed 23
                   :error-get-subvol-rootref-failed 24
                   :error-ino-lookup-user-failed 25
                   :error-fs-info-failed 26)

(define-alien-routine btrfs-util-strerror c-string (err btrfs-util-error))
(define-alien-routine btrfs-util-sync btrfs-util-error (path c-string))
(define-alien-routine btrfs-util-fs-sync btrfs-util-error (path c-string))
(define-alien-routine btrfs-util-sync-fd btrfs-util-error (fd int))
(define-alien-routine btrfs-util-fs-sync-fd btrfs-util-error (fd int))
(define-alien-routine btrfs-util-start-sync btrfs-util-error
  (path c-string)
  (transid (* (unsigned 64))))
(define-alien-routine btrfs-util-fs-start-sync btrfs-util-error
  (path c-string)
  (transid (* (unsigned 64))))
(define-alien-routine btrfs-util-fs-start-sync-fd btrfs-util-error
  (fd int)
  (transid (* (unsigned 64))))

(define-alien-routine btrfs-util-wait-sync btrfs-util-error (path c-string) (transid (unsigned 64)))

(define-alien-routine btrfs-util-wait-sync-fd btrfs-util-error (fd int) (transid (unsigned 64)))

(define-alien-routine btrfs-util-fs-wait-sync-fd btrfs-util-error (fd int) (transid (unsigned 64)))

(define-alien-routine btrfs-util-is-subvolume btrfs-util-error (path c-string))

(define-alien-routine btrfs-util-subvolume-is-valid btrfs-util-error (path c-string))

(define-alien-routine btrfs-util-is-subvolume-fd btrfs-util-error (fd int))

(define-alien-routine btrfs-util-subvolume-is-valid-fd btrfs-util-error (fd int))

(define-alien-routine btrfs-util-subvolume-id btrfs-util-error (path c-string) (id-ret (* (unsigned 64))))

;; TODO 2024-06-30: L203
(define-alien-routine btrfs-util-subvolume-get-id btrfs-util-error (path c-string) (id-ret (* (unsigned 64))))
