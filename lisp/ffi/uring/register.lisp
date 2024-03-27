;;; uring/register.lisp --- IO Register syscalls

;;

;;; Code:
(in-package :uring)

(defconstant +io-probe-count+ 256)

(defstruct io-probe
  (probe (allocate-io-uring-probe) :type io-uring-probe)
  (ops (make-array #.+io-probe-count+ :element-type 'io-uring-probe-op)
   :type (array io-uring-probe-op (#.+io-probe-count+))))

;; op-supported-p

;; An allowed feature of io_uring. Set allowed features with register_restrictions.
(defstruct io-restriction (restriction nil :type io-uring-restriction))

;; register-op, sqe-op, sqe-flags-allowed, sqe-flags-required, 

;; used with register_files_update to skip file-descriptors.
(defconstant +io-skip-file+ ioring-register-files-skip)
