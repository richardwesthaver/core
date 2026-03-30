;;; uring/register.lisp --- IO Register syscalls

;;

;;; Code:
(in-package :uring)

(defconstant +io-probe-count+ 256)

(defstruct io-probe
  (probe (io-uring-get-probe) :type (alien (* io-uring-probe)))
  (ops (make-array #.+io-probe-count+)
   :type (array (alien io-uring-probe-op) (#.+io-probe-count+))))

;; op-supported-p

;; An allowed feature of io_uring. Set allowed features with register_restrictions.
(defstruct io-restriction (restriction (make-alien io-uring-restriction) :type (alien (* io-uring-restriction))))

;; register-op, sqe-op, sqe-flags-allowed, sqe-flags-required

;; used with register_files_update to skip file-descriptors.
(defconstant +io-skip-file+ ioring-register-files-skip)
