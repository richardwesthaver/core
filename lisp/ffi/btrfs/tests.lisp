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
  (is (equal "Cannot allocate memory" (btrfs-util-strerror (btrfs-util-error :no-memory))))
  (is (btrfs::allocate-btrfs-qgroup-limit))
  (is (btrfs::allocate-btrfs-util-subvolume-info)))

;; for a complete test we need to be have fs permissions.

;; To run these tests, pass a filesystem path as fixture input when running as
;; root.

(deftest basic (:skip t)
  ;; will only work when root filesystem is BTRFS
  (is (zerop (btrfs::btrfs-util-subvolume-is-valid "/")))
  (is (zerop (btrfs::btrfs-util-fs-sync "/")))
  ;; (sb-alien:with-alien ((id (unsigned 64)))
  ;;   (btrfs-util-strerror (btrfs::btrfs-util-subvolume-get-default "/" (addr id))))
  (sb-alien:with-alien ((id (unsigned 64))
                        (path c-string (make-alien-string ""))
                        (iter (* btrfs-util-subvolume-iterator)))
    (btrfs-util-subvolume-iter-create "/" 0 0 (addr iter))
    (btrfs-util-subvolume-iter-destroy iter)))
  
