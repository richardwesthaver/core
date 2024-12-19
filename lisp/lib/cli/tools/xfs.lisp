;;; xfs.lisp --- XFS Tools

;; 

;;; Code:
(in-package :cli/tools/xfs)

(deferror xfs-error (simple-error) () (:auto t))

(defparameter *xfs* (find-exe "clang"))
