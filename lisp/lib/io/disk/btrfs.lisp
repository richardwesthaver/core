;;; btrfs.lisp --- BTRFS API

;; High-level BTRFS backend for IO/DISK

;;; Code:
(in-package :io/disk/btrfs)

(defclass btrfs-disk (disk) ())

(defclass btrfs-partition (disk-partition) ())

(defclass btrfs-subvolume (disk-subvolume) ())

(defclass btrfs-snapshot (disk-snapshot) ())
