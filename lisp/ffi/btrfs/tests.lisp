;;; src/fs/btrfs/tests.lisp --- BTRFS common-lisp tests

;;; Code:
(defpackage btrfs/tests
  (:use :cl :std :rt :btrfs :sb-alien))
(in-package :btrfs/tests)

(defsuite :btrfs)
(in-suite :btrfs)

(when (zerop (parse-integer (with-output-to-string (str) (sb-ext:process-output (sb-ext:run-program "id" (list "-u") :search t :output str)) 0)))
  (pushnew :sudo *features*))

(load-btrfs)
(load-btrfsutil)

(defvar *test-btrfs-pathname* (directory-path (symbol-name (gensym "/tmp/btrfs"))))

(deftest sanity ()
  (is (typep +btrfs-lib-version+ 'fixnum))
  (is (eq 0 (btrfs-util-error :ok)))
  (is (equal "Cannot allocate memory" (btrfs-util-strerror (btrfs-util-error :no-memory))))
  (is (btrfs::allocate-btrfs-qgroup-limit))
  (is (btrfs::allocate-btrfs-util-subvolume-info)))

(deftest basic ()
  (is (zerop (btrfs::btrfs-util-subvolume-is-valid "/")))
  (is (zerop (btrfs::btrfs-util-fs-sync "/")))
  #+sudo
  (sb-alien:with-alien ((id (unsigned 64)))
    (btrfs::btrfs-util-subvolume-get-default "/home/ellis" (addr id)))
  (sb-alien:with-alien ((id (unsigned 64))
                        (path c-string (make-alien-string ""))
                        (iter (* btrfs-util-subvolume-iterator)))
    (is (zerop (btrfs-util-subvolume-iter-create "/" 0 0 (addr iter))))
    (isnt (btrfs-util-subvolume-iter-destroy iter))))
