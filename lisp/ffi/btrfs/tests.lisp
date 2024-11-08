;;; src/fs/btrfs/tests.lisp --- BTRFS common-lisp tests

;;; Code:
(defpackage btrfs/tests
  (:use :cl :std :rt :btrfs :sb-alien))
(in-package :btrfs/tests)

(defsuite :btrfs)
(in-suite :btrfs)

(load-btrfs)
(load-btrfsutil)

(defvar *test-btrfs-pathname* (directory-path (symbol-name (gensym "/tmp/btrfs"))))

(deftest sanity ()
  (is (typep +btrfs-lib-version+ 'fixnum))
  (is (eq 0 (btrfs-util-error :ok)))
  (is (equal "Cannot allocate memory" (btrfs-util-strerror (btrfs-util-error :no-memory)))))
  
(deftest basic ()
  (is (zerop (btrfs::btrfs-util-subvolume-is-valid "/")))
  (is (zerop (btrfs::btrfs-util-fs-sync "/")))
  (sb-alien:with-alien ((id (unsigned 64))
                        (path c-string (make-alien-string ""))
                        (iter (* btrfs-util-subvolume-iterator)))
    (is (zerop (btrfs-util-subvolume-iter-create "/" 0 0 (addr iter))))
    (isnt (btrfs-util-subvolume-iter-destroy iter))))

(deftest root-fs (:skip (or (not (sudo-p)) (not (zerop (btrfs-util-subvolume-is-valid "/")))))
  ;; will only work on a BTRFS Linux root partition
  (let ((path "/"))
    (with-alien ((id (unsigned 64)))
      (iszero (btrfs-util-fs-sync path))
      ;; returns 0 on success
      (iszero (btrfs-util-subvolume-is-valid path))
      (iszero (btrfs-util-subvolume-get-id path (addr id)))
      (with-alien ((path1 c-string))
        (btrfs-util-subvolume-get-path "/" id (addr path1))
        (isempty path1))
      (with-alien ((info btrfs-util-subvolume-info))
        ;; :error-search-failed
        (iszero (btrfs-util-subvolume-get-info "/" id (addr info))))
      ;; T
      (with-alien ((ret boolean))
        (iszero (btrfs-util-subvolume-get-read-only "/" (addr ret))))
      ;; create snapshot
      (with-alien ((qgroups (* btrfs-util-qgroup-inherit)))
        (iszero (btrfs-util-qgroup-inherit-create 0 (addr qgroups)))
        (iszero (btrfs-util-subvolume-snapshot "/" "/tmp/btrfs-sn1" 0 nil qgroups))))))

#|
Subvolume iterators require appropriate privilege (CAP_SYS_ADMIN) unless @top
is zero and the kernel supports BTRFS_IOC_GET_SUBVOL_ROOTREF and
BTRFS_IOC_INO_LOOKUP_USER (kernel >= 4.18). In this case, subvolumes which
cannot be accessed (e.g., due to permissions or other mounts) will be
skipped.

The returned iterator must be freed with
btrfs_util_subvolume_iter_destroy().

Return: %BTRFS_UTIL_OK on success, non-zero error code on failure.
|#

(deftest subvolume-iter (:skip (or (not (sudo-p)) (not (zerop (btrfs-util-subvolume-is-valid "/")))))
  (let ((path "/"))
    (with-alien ((iter (* btrfs-util-subvolume-iterator)))
      (iszero (btrfs-util-subvolume-iter-create path 0 0 (addr iter)))
      (isnt (btrfs-util-subvolume-iter-destroy iter))
      (with-alien ((pret c-string) (id unsigned-long))
        ;; root ref
        (iszero (btrfs-util-subvolume-iter-next iter (addr pret) (addr id)))))))

