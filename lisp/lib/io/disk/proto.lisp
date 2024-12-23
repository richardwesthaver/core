;;; disk.lisp --- Disk IO

;; A shared Disk storage interface.

;;; Commentary:

;; Note that many disk operations require root privileges. This package does
;; not handle privilege escalation.

;;; Code:
(in-package :io/disk)

;;; Vars
(defvar *filesystem-backends* (list :btrfs :ext4 :xfs))
(defvar *default-filesystem* :btrfs)

;;; Conditions
(define-condition disk-condition () ())

(defun load-filesystem-backend (&optional (fs *default-filesystem*))
  (case fs
    (:btrfs (load-btrfs) (load-btrfsutil) t)
    (:xfs t)))

(defclass disk () ())

(defclass disk-partition () ())

(defclass disk-subvolume () ())

(defclass disk-snapshot () ())

(defgeneric snapshot (self &rest args &key &allow-other-keys))
(defgeneric sync (self &rest args &key &allow-other-keys))
