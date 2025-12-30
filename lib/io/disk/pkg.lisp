;;; pkg.lisp --- Disk-based IO

;; 

;;; Code:
(defpackage :io/disk
  (:nicknames :disk)
  (:use :cl :std :io/proto :btrfs :sb-alien)
  (:shadowing-import-from :std/os :dir :fsname :opts :freq :passno)
  (:export
   #:*default-filesystem*
   #:*filesystem-backends*
   #:disk-condition
   #:load-filesystem-backend
   :disk
   :disk-partition
   #:disk-snapshot
   #:disk-subvolume
   :list-disks
   :list-disk-info
   :disk-space
   :disk-total-space
   :disk-available-space
   :disk-free-space
   #:statvfs
   #:disk-info
   #:mountpoint-get
   #:mountpoint-device
   #:mountpoint-fstype
   #:mountpoint-options
   #:fsblkcnt-t
   #:fsfilcnt-t
   #:disk-use-percent
   #:mountpoint-directory))

(defpackage :io/disk/btrfs
  (:nicknames :disk/btrfs)
  (:use :cl :std :io/proto :btrfs :io/disk :sb-alien)
  (:export
   :btrfs-subvolume
   :btrfs-disk
   :btrfs-subvolumes
   :btrfs-default-subvolume
   :btrfs-snapshot
   :subvolume-valid-p
   :btrfs-partition
   :btrfs-simple-error
   :btrfs-error
   :load-btrfs-libs))
