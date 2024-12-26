;;; pkg.lisp --- Disk-based IO

;; 

;;; Code:
(defpackage :io/disk
  (:nicknames :disk)
  (:use :cl :std :io/proto :btrfs :sb-alien)
  (:export
   #:*default-filesystem*
   #:*filesystem-backends*
   #:disk-condition
   #:load-filesystem-backend
   :disk
   :disk-partition
   #:sync
   #:snapshot
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
   #:mnt-fsname
   #:mnt-dir
   #:mnt-type
   #:mnt-opts
   #:mnt-freq
   #:mnt-passno
   #:mntent
   #:mountpoint-get
   #:mountpoint-device
   #:mountpoint-fstype
   #:mountpoint-options
   #:fsblkcnt-t
   #:fsfilcnt-t))

(defpackage :io/disk/btrfs
  (:nicknames :disk/btrfs)
  (:use :cl :std :io/proto :btrfs :io/disk)
  (:export
   :btrfs-subvolume
   :btrfs-disk))
