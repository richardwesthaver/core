;;; uring/alien.lisp --- URING alien routines

;;

;;; Code:
(in-package :uring)

;;; barrier.h
;; (defun io-uring-write-once (var val))
;; (defun io-uring-read-once (var))
;; (defun io-uring-smp-store-release (p v))
;; (defun io-uring-smp-load-acquire (p))
;; (defun io-uring-smp-mb ())

;;; liburing.h
(defalien-int io-uring-major-version)
(defalien-int io-uring-minor-version)
(defalien-int io-uring-check-version (major int) (minor int))

;;; Syscalls
;; register, setup, enter

(define-alien-routine io-uring-get-probe-ring (* io-uring-probe) (ring (* (struct io-uring))))
(define-alien-routine io-uring-get-probe (* io-uring-probe))
(define-alien-routine io-uring-free-probe void (* io-uring-probe))

(defalien-int io-uring-opcode-supported (p (* (struct io-uring-probe))) (op int))
(defalien-int io-uring-queue-init-mem
  (entries unsigned)
  (ring (* (struct io-uring)))
  (p (* (struct io-uring-params)))
  (buf (* t)) (buf-size size-t))
(defalien-int io-uring-queue-init-params
  (entries unsigned)
  (ring (* (struct io-uring)))
  (p (* (struct io-uring-params))))
(defalien-int io-uring-queue-init (entries int) (ring (* (struct io-uring))) (flags unsigned))
(defalien-int io-uring-queue-mmap (fd int) (p (* (struct io-uring-params))) (ring (* (struct io-uring))))
(defalien-int io-uring-ring-dontfork (ring (* (struct io-uring))))
(defalien-int io-uring-queue-exit (ring (* (struct io-uring))))
(defalien-int io-uring-peek-batch-cqe (ring (* (struct io-uring))) (cqes (array (* (struct io-uring-cqe)))) (count unsigned))
(defalien-int io-uring-wait-cqes
  (ring (* (struct io-uring)))
  (cqe-ptr (* (* (struct io-uring-cqe))))
  (wait-nr unsigned)
  (ts (* (struct kernel-timespec)))
  (sigmask (* (struct sigset-t)))) ;; maybe should be (* t)?
(defalien-int io-uring-wait-cqe-timeout
  (ring (* (struct io-uring)))
  (cqe-ptr (* (* (struct io-uring-cqe))))
  (ts (* (struct kernel-timespec))))
(defalien-int io-uring-submit (ring (* (struct io-uring))))
(defalien-int io-uring-submit-and-wait 
  (ring (* (struct io-uring)))
  (wait-nr unsigned))
(defalien-int io-uring-submit-and-wait-timeout
  (ring (* (struct io-uring)))
  (cqe-ptr (* (* (struct io-uring-cqe))))
  (ts (* (struct kernel-timespec))))
(defalien-int io-uring-register-buffers
  (ring (* (struct io-uring)))
  (iovecs (* (struct iovec)))
  (nr-iovecs unsigned))
(defalien-int io-uring-register-buffer-tags
  (ring (* (struct io-uring)))
  (iovecs (* (struct iovec)))
  (tags (array unsigned-long))
  (nr unsigned))
(defalien-int io-uring-register-buffer-sparse
  (ring (* (struct io-uring)))
  (nr unsigned))
(defalien-int io-uring-register-buffer-update-tag
  (ring (* (struct io-uring)))
  (off unsigned)
  (iovecs (* (struct iovec)))
  (tags (array unsigned-long))
  (nr unsigned))
(defalien-int io-uring-unregister-buffers (ring (* (struct io-uring))))
  
(defalien-int io-uring-register-files
  (ring (* (struct io-uring)))
  (files (array int))
  (nr-files unsigned))

(defalien-int io-uring-register-files-tags
  (ring (* (struct io-uring)))
  (files (array int))
  (tags (array unsigned-long))
  (nr unsigned))

(defalien-int io-uring-register-files-sparse
  (ring (* (struct io-uring)))
  (nr unsigned))

(defalien-int io-uring-register-files-update-tags
  (ring (* (struct io-uring)))
  (off unsigned)
  (files (array int))
  (tags (array unsigned-long))
  (nr-files unsigned))
(defalien-int io-uring-unregister-files (ring (* (struct io-uring))))

(defalien-int io-uring-register
  (fd int)
  (opcode unsigned-int)
  (args (* t))
  (nr-args unsigned-int))

;;...

(defalien-int io-uring-enable-rings (ring (* (struct io-uring))))
(defalien-int io-uring-sqring-wait (ring (* (struct io-uring))))

;;...
(defalien-int io-uring-setup
  (entries unsigned-int)
  (p (* (struct io-uring-params))))

(defalien-int io-uring-enter
  (fd int)
  (to-submit unsigned-int)
  (min-complete unsigned-int)
  (flags unsigned-int)
  (arg (* t))
  (size unsigned-long))

(define-alien-routine io-uring-setup-buf-ring (* (struct io-uring-buf-ring))
  (ring (* (struct io-uring)))
  (nentries unsigned-int)
  (bgid int)
  (flags unsigned-int)
  (ret (* int)))
(defalien-int io-uring-free-buf-ring
    (ring (* (struct io-uring)))
  (br (* (struct io-uring-buf-ring)))
  (nentries unsigned-int)
  (bgid int))
;;...

;; peek-cqe wait-cqe get-sqe
;; io-uring-buf-ring-init
