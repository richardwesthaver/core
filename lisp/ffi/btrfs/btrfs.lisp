;;; src/fs/btrfs/btrfs.lisp --- BTRFS common-lisp API

;; This package contains FFI bindings to the BTRFS C libraries libbtrfs and
;; libbtrfsutil as well as some additional core routines from Rust.

;;; Commentary:

;; BTRFS is a core component of the NAS-T stack. We might even consider NAS-T as a
;; wrapper around BTRFS APIs in the same we we could say that TrueNAS is a wrapper
;; around ZFS.

;; NOTE 2023-09-03: currently the app has no concrete use-cases for accessing BTRFS APIs
;; directly from lisp. This will inevitably change, and we want the bindings for
;; debugging and experimentation.

;;; Code:
(defpackage btrfs
  (:use :cl :sb-alien)
  (:export
   :btrfs-shared-objects
   :btrfs-lib-path
   :load-btrfs :unload-btrfs
   :load-btrfsutil :unload-btrfsutil
   :define-btrfs-ioctl))

(in-package :btrfs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar btrfs-shared-objects
    (list '(:btrfs "/usr/lib/libbtrfs.so")
          '(:btrfsutil "/usr/lib/libbtrfsutil.so")))
  
  (defun btrfs-lib-path (lib) (cadr (assoc lib btrfs-shared-objects))))
                
(defmacro when-lib-exists-p ((sym lib) &body body)
  `(let ((,sym ,(btrfs-lib-path lib)))
    (when (uiop:file-exists-p ,sym) ,@body)))

(defun load-btrfs (&optional save)
  "Open 'libbtrfs' using `dlopen'. exposing the C API to the current Lisp image."
  (when-lib-exists-p (l :btrfs) (load-shared-object l :dont-save (not save))))

(defun unload-btrfs ()
  "Close 'libbtrfs' using `dlclose'."
  (unload-shared-object (btrfs-lib-path :btrfs)))

(defun load-btrfsutil ()
  "Open 'libbtrfsutil' using `dlopen'. exposing the C API to the current Lisp image."
  (when-lib-exists-p (l :btrfsutil) (load-shared-object l)))

(defun unload-btrfsutil ()
  "Close 'libbtrfsutil' using `dlclose'."
  (unload-shared-object (btrfs-lib-path :btrfsutil)))

(defmacro define-btrfs-ioctl () "Define a wrapper for IOCTLs exposed by BTRFS.")
