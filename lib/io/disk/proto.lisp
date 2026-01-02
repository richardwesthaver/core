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
(defvar *disks* nil)
;;; Conditions
(define-condition disk-condition () ())

(defun load-filesystem-backend (&optional (fs *default-filesystem*))
  (case fs
    (:btrfs (load-btrfs) (load-btrfsutil))))

(defclass disk () ((path :initarg :path :initform #p"/" :accessor path)))

(defclass disk-partition () ())

(defclass disk-subvolume () ())

(defclass disk-snapshot () ())

(defmethod init ((self (eql :disk)) &key (backend *default-filesystem*))
  (load-filesystem-backend backend))
