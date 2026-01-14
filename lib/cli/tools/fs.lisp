;;; fs.lisp --- Filesystem Tools

;; 

;;; Code:
(in-package :cli/tools/fs)

#+todo
(define-cli-tool :xfs_info (&rest args)
  (let ((proc (sb-ext:run-program *xfs_info* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (xfs-info-error "XFS_INFO command failed: ~A ~A" *xfs-info* (or args "")))))

(define-cli-tool :btrfs (&rest args)
  (let ((proc (sb-ext:run-program *btrfs* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (btrfs-error "BTRFS command failed: ~A ~A" *btrfs* (or args "")))))

;; (run-btrfs "filesystem" "usage" "/")
