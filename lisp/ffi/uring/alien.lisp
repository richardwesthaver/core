;;; uring/alien.lisp --- supplementary alien types

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

(defconstant +nr-io-uring-setup+ 425)
(defconstant +nr-io-uring-enter+ 426)
(defconstant +nr-io-uring-register+ 427)

(define-alien-type nil
  (struct io-uring-sq
          (khead (* unsigned-int))
          (ktail (* unsigned-int))          
          (kring-mask (* unsigned-int))
          (kring-entries (* unsigned-int))
          (kflags (* unsigned-int))
          (kdropped (* unsigned-int))
          (array (* unsigned-int))
          (sqes (* (struct io-uring-sqe)))
          (sqe-head unsigned-int)
          (sqe-tail unsigned-int)
          (ring-sz sb-unix:size-t)
          (ring-ptr (* t))
          (ring-mask unsigned-int)
          (ring-entries unsigned-int)
          (pad (array unsigned-int 2))))

(define-alien-type io-uring-sq* (* (struct io-uring-sq)))

(define-alien-type nil
  (struct io-uring-cq
          (khead (* unsigned-int))
          (ktail (* unsigned-int))
          (kring-mask (* unsigned-int))
          (kring-entries (* unsigned-int))
          (kflags (* unsigned-int))
          (koverflow (* unsigned-int))
          (cqes (* (struct io-uring-cqe)))
          (ring-sz sb-unix:size-t)
          (ring-ptr (* t))
          (ring-mask unsigned-int)
          (ring-entries unsigned-int)
          (pad (array unsigned-int 2))))

(define-alien-type io-uring-cq* (* (struct io-uring-cq)))

(define-alien-type nil
    (struct io-uring
            (sq (struct io-uring-sq))
            (cq (struct io-uring-cq))
            (flags unsigned-int)
            (ring-fd int)
            (features unsigned-int)
            (enter-ring-fd int)
            (int-flags char)
            (pad (array char 3))
            (pad2 unsigned-int)))

(define-alien-routine io-uring-get-probe-ring (* io-uring-probe) (ring (* (struct io-uring))))
(define-alien-routine io-uring-get-probe (* io-uring-probe))
(define-alien-routine io-uring-free-probe void (p (* (struct io-uring-probe))))

;; (defalien-int io-uring-opcode-supported (p (* (struct io-uring-probe))) (op int))
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
(def-with-ring io-uring-ring-dontfork)
(def-with-ring io-uring-queue-exit)
(def-with-ring io-uring-peek-batch-cqe (cqes (array (* (struct io-uring-cqe)))) (count unsigned))
(def-with-ring io-uring-wait-cqes
  (cqe-ptr (* (* (struct io-uring-cqe))))
  (wait-nr unsigned)
  (ts (* (struct kernel-timespec)))
  (sigmask (* (struct sigset-t)))) ;; maybe should be (* t)?
(def-with-ring io-uring-wait-cqe-timeout
  (cqe-ptr (* (* (struct io-uring-cqe))))
  (ts (* (struct kernel-timespec))))
(def-with-ring io-uring-submit)
(def-with-ring io-uring-submit-and-wait 
  (wait-nr unsigned))
(def-with-ring io-uring-submit-and-wait-timeout
  (cqe-ptr (* (* (struct io-uring-cqe))))
  (ts (* (struct kernel-timespec))))
(def-with-ring io-uring-register-buffers
  (iovecs (* (struct iovec)))
  (nr-iovecs unsigned))
(def-with-ring io-uring-register-buffer-tags
  (iovecs (* (struct iovec)))
  (tags (array unsigned-long))
  (nr unsigned))
(def-with-ring io-uring-register-buffer-sparse
  (nr unsigned))
(def-with-ring io-uring-register-buffer-update-tag
  (off unsigned)
  (iovecs (* (struct iovec)))
  (tags (array unsigned-long))
  (nr unsigned))
(def-with-ring io-uring-unregister-buffers)
(def-with-ring io-uring-register-files
  (files (array int))
  (nr-files unsigned))
(def-with-ring io-uring-register-files-tags
  (files (array int))
  (tags (array unsigned-long))
  (nr unsigned))
(def-with-ring io-uring-register-files-sparse
  (nr unsigned))
(def-with-ring io-uring-register-files-update-tag
  (off unsigned)
  (files (array int))
  (tags (array unsigned-long))
  (nr-files unsigned))
(def-with-ring io-uring-unregister-files)
(def-with-ring io-uring-register-files-update
  (off unsigned)
  (files (array int))
  (tags (array unsigned-long))
  (nr-files unsigned))
(def-with-ring io-uring-register-eventfd (fd int))
(def-with-ring io-uring-register-eventfd-async (fd int))
(def-with-ring io-uring-unregister-eventfd)
(def-with-ring io-uring-register-probe
  (p (* (struct io-uring-probe)))
  (nr unsigned))
(def-with-ring io-uring-register-personality)
(def-with-ring io-uring-unregister-personality (fd int))
(def-with-ring io-uring-register-restrictions (res (array (struct io-uring-restriction))) (nr-res unsigned-int))
;; (defalien-int io-uring-register
;;   (fd int)
;;   (opcode unsigned-int)
;;   (args (* t))
;;   (nr-args unsigned-int))
(def-with-ring io-uring-enable-rings)
(def-with-ring __io-uring-sqring-wait) ;;fixme
(def-with-ring io-uring-register-iowq-aff (cpusz size-t) (mask (* (struct cpu-set-t))))
(def-with-ring io-uring-unregister-iowq-aff)
(def-with-ring io-uring-register-iowq-max-workers (values (array unsigned-int)))
(def-with-ring io-uring-register-ring-fd)
(def-with-ring io-uring-unregister-ring-fd)
(def-with-ring io-uring-close-ring-fd)
(def-with-ring io-uring-register-buf-ring
  (reg (* (struct io-uring-buf-reg))) (flags unsigned-int))
(def-with-ring io-uring-unregister-buf-ring (bgid int))
(def-with-ring io-uring-register-sync-cancel (reg (* (struct io-uring-sync-cancel-reg))))
(def-with-ring io-uring-register-file-alloc-range (off unsigned) (len unsigned))
(def-with-ring io-uring-get-events)
(def-with-ring io-uring-submit-and-get-events)

;;; Syscalls
(defalien-int io-uring-enter
  (fd int)
  (to-submit unsigned-int)
  (min-complete unsigned-int)
  (flags unsigned-int)
  (sig (* (struct sigset-t))))

(defalien-int io-uring-enter2
  (fd int)
  (to-submit unsigned-int)
  (min-complete unsigned-int)
  (flags unsigned-int)
  (sig (* (struct sigset-t)))
  (sz size-t))
(defalien-int io-uring-setup
  (entries unsigned-int)
  (p (* (struct io-uring-params))))
(defalien-int io-uring-register (fd unsigned-int) (opcode unsigned-int) (arg (* t)) (nr-args unsigned-int))

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
;; __io_uring_get_cqe

;; peek-cqe wait-cqe get-sqe
;; io-uring-buf-ring-init

;;..
(define-alien-routine io-uring-mlock-size ssize-t (entries unsigned) (flags unsigned))
(define-alien-routine io-uring-mlock-size-params ssize-t (entries unsigned) (p (* (struct io-uring-params))))
