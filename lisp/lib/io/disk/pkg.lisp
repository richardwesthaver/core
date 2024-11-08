;;; pkg.lisp --- Disk-based IO

;; 

;;; Code:
(defpackage :io/disk
  (:nicknames :disk)
  (:use :cl :std :io/proto :btrfs)
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
   #:disk-subvolume))

(defpackage :io/disk/btrfs
  (:nicknames :disk/btrfs)
  (:use :cl :std :io/proto :btrfs :io/disk)
  (:export
   :btrfs-subvolume
   :btrfs-disk))
