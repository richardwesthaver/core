;;; btrfs.lisp --- BTRFS API

;; High-level BTRFS backend for IO/DISK

;;; Code:
(in-package :io/disk/btrfs)

(define-condition btrfs-error (io-error) ())

(deferror btrfs-simple-error (simple-error btrfs-error) () (:auto t))

(defclass btrfs-disk (disk) ())

(defclass btrfs-partition (disk-partition) ())

(defclass btrfs-subvolume (disk-subvolume disk) ())

(defun subvolume-valid-p (subvol)
  (etypecase subvol
    (string (eql :ok (btrfs-util-subvolume-is-valid subvol)))
    (pathname (eql :ok (btrfs-util-subvolume-is-valid (namestring subvol))))
    (disk (eql :ok (btrfs-util-subvolume-is-valid (namestring (path subvol)))))))

(defclass btrfs-snapshot (disk-snapshot disk) ())

(defmethods sync 
  (((self btrfs-subvolume) &key)
   (btrfs-util-fs-sync (namestring (path self))))
  (((self btrfs-disk) &key)
   (btrfs-util-fs-sync (namestring (path self)))))
         
(defun btrfs-subvolumes (path)
  (when (subvolume-valid-p path)
    (sb-alien:with-alien ((iter (* btrfs-util-subvolume-iterator)))
      (unwind-protect
           (progn
             (btrfs-util-subvolume-iter-create path 0 0 (sb-alien:addr iter))
             (with-alien ((path c-string)
                          (id (unsigned 64)))
               (loop while (eql :ok (btrfs-util-subvolume-iter-next 
                                     iter (addr path) (addr id)))
                     collect (cons path id))))
        (btrfs-util-subvolume-iter-destroy iter)))))

(defun btrfs-default-subvolume (path)
  (with-alien ((id (unsigned 64)))
    (let ((res (btrfs-util-subvolume-get-default path (addr id))))
      (if (eql res :ok)
          id
          (btrfs-simple-error (btrfs-util-strerror res))))))
