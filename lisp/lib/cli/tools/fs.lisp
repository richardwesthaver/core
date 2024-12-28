;;; fs.lisp --- Filesystem Tools

;; 

;;; Code:
(in-package :cli/tools/fs)

(deferror fs-error (simple-error) () (:auto t))

(defparameter *xfs-info* (find-exe "xfs_info"))
