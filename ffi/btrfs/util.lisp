;;; util.lisp --- BtrfsUtil Alien Bindings

;; 

;;; Code:
(in-package :btrfs)

(define-opaque btrfs-util-qgroup-inherit t)
(define-opaque btrfs-util-subvolume-iterator t)

(define-alien-enum (btrfs-util-error int)
                   :ok 0
                   :stop-iteration 1
                   :no-memory 2
                   :invalid-argument 3
                   :not-btrfs 4
                   :not-subvolume 5
                   :subvolume-not-found 6
                   :error-open-failed 7
                   :error-rmdir-failed 8
                   :error-unlink-failed 9
                   :error-stat-failed 10
                   :error-statfs-failed 11
                   :error-search-failed 12
                   :error-ino-lookup-failed 13
                   :error-subvol-getflags-failed 14
                   :error-subvol-setflags-failed 15
                   :error-subvol-create-failed 16
                   :error-snap-create-failed 17
                   :error-snap-destroy-failed 18
                   :error-default-subvol-failed 19
                   :error-sync-failed 20
                   :error-start-sync-failed 21
                   :error-wait-sync-failed 22
                   :error-get-subvol-info-failed 23
                   :error-get-subvol-rootref-failed 24
                   :error-ino-lookup-user-failed 25
                   :error-fs-info-failed 26)

(define-alien-routine btrfs-util-strerror c-string (err btrfs-util-error))

(define-alien-routine btrfs-util-subvolume-iter-destroy void
  (iter (* btrfs-util-subvolume-iterator)))

(define-alien-routine btrfs-util-subvolume-iterator-get-fd int
  (iter (* btrfs-util-subvolume-iterator)))

(define-alien-routine btrfs-util-qgroup-inherit-destroy void
  (inherit (* btrfs-util-qgroup-inherit)))

(macrolet ((def (name &rest args)
               `(progn
                  (define-alien-routine ,name btrfs-util-error ,@args)
                  (export ',name))))
  (def btrfs-util-fs-sync (path c-string))
  (def btrfs-util-fs-sync-fd (fd int))
  (def btrfs-util-fs-start-sync
      (path c-string)
      (transid (* (unsigned 64))))
  (def btrfs-util-fs-start-sync-fd
      (fd int)
      (transid (* (unsigned 64))))
  (def btrfs-util-fs-wait-sync (path c-string) (transid (unsigned 64)))
  (def btrfs-util-fs-wait-sync-fd (fd int) (transid (unsigned 64)))
  (def btrfs-util-subvolume-is-valid (path c-string))
  (def btrfs-util-subvolume-is-valid-fd (fd int))
  (def btrfs-util-subvolume-get-id (path c-string) (id-ret (* (unsigned 64))))
  (def btrfs-util-subvolume-get-id-fd (fd int) (id-ret (* (unsigned 64))))
  (def btrfs-util-subvolume-get-path
      (path c-string)
      (id (unsigned 64))
    (path-ret (* c-string)))
  (def btrfs-util-subvolume-get-path-fd
      (fd int)
      (id (unsigned 64))
    (path-ret (* c-string)))
  (def btrfs-util-subvolume-get-info
      (path c-string)
      (id (unsigned 64))
    (subvol (* btrfs-util-subvolume-info)))
  (def btrfs-util-subvolume-get-info-fd
      (fd int)
      (id (unsigned 64))
    (subvol (* btrfs-util-subvolume-info)))
  (def btrfs-util-subvolume-get-read-only
      (path c-string)
      (ret (* boolean)))
  (def btrfs-util-subvolume-get-read-only-fd
      (fd int)
      (ret (* boolean)))
  (def btrfs-util-subvolume-set-read-only
      (path c-string)
      (read-only boolean))
  (def btrfs-util-subvolume-set-read-only-fd
      (fd int)
      (read-only boolean))
  (def btrfs-util-subvolume-get-default
      (path c-string)
      (id-ret (* (unsigned 64))))
  (def btrfs-util-subvolume-get-default-fd
      (fd int)
      (id-ret (* (unsigned 64))))
  (def btrfs-util-subvolume-set-default
      (path c-string)
      (id (unsigned 64)))
  (def btrfs-util-subvolume-set-default-fd
      (fd int)
      (id (unsigned 64)))
  (def btrfs-util-subvolume-create
      (path c-string)
      (flags (* int))
    (unused (* (unsigned 64)))
    (qgroup-inherit (* btrfs-util-qgroup-inherit)))
  (def btrfs-util-subvolume-create-fd
      (fd int)
      (name c-string)
    (flags int)
    (unused (* (unsigned 64)))
    (qgroup-inherit (* btrfs-util-qgroup-inherit)))
  (def btrfs-util-subvolume-snapshot
      (source c-string)
      (path c-string)
    (flags int)
    (unused (* (unsigned 64)))
    (qgroup-inherit (* btrfs-util-qgroup-inherit)))
  (def btrfs-util-subvolume-snapshot-fd
      (fd int)
      (path c-string)
    (flags int)
    (unused (* (unsigned 64)))
    (qgroup-inherit (* btrfs-util-qgroup-inherit)))
  (def btrfs-util-subvolume-snapshot-fd2
      (fd int)
      (parent-fd int)
    (name c-string)
    (flags int)
    (unused (* (unsigned 64)))
    (qgroup-inherit (* btrfs-util-qgroup-inherit)))
  (def btrfs-util-subvolume-delete
      (path c-string)
      (flags int))
  (def btrfs-util-subvolume-delete-fd
      (parent-fd int)
      (name c-string)
    (flags int))
  (def btrfs-util-subvolume-delete-by-id-fd
      (fd int)
      (subvolid (unsigned 64)))
  (def btrfs-util-subvolume-iter-create
      (path c-string)
      (top (unsigned 64))
    (flags int)
    (ret (* (* btrfs-util-subvolume-iterator))))
  (def btrfs-util-subvolume-iter-create-fd
      (fd int)
      (top (unsigned 64))
    (flags int)
    (ret (* (* btrfs-util-subvolume-iterator))))
  (def btrfs-util-subvolume-iter-next
      (iter (* btrfs-util-subvolume-iterator))
      (path-ret (* c-string))
    (id-ret (* (unsigned 64))))
  (def btrfs-util-subvolume-iter-next-info
      (iter (* btrfs-util-subvolume-iterator))
      (path-ret (* c-string))
    (subvol (* btrfs-util-subvolume-info)))
  (def btrfs-util-subvolume-list-deleted
      (path c-string)
      (ids (* (* (unsigned 64))))
    (n (* size-t)))
  (def btrfs-util-subvolume-list-deleted-fd
      (fd int)
      (ids (* (* (unsigned 64))))
    (n (* size-t)))
  (def btrfs-util-qgroup-inherit-create
      (flags int)
      (ret (* (* btrfs-util-qgroup-inherit))))
  (def btrfs-util-qgroup-inherit-add-group
      (inherit (* (* btrfs-util-qgroup-inherit)))
      (qgroupid (unsigned 64)))
  (def btrfs-util-qgroup-inherit-get-groups
      (inherit (* btrfs-util-qgroup-inherit))
      (groups (* (* (unsigned 64))))
    (n (* size-t))))
