;;; uring/alien.lisp --- supplementary alien types

;;

;;; Code:
(in-package :uring)

(define-alien-type io-uring-napi
  (struct io-uring-napi
    (busy-poll-to unsigned-int)
    (prefer-busy-poll unsigned-char)
    (pad (array unsigned-char 3))
    (resv unsigned-long)))

;; ZCRX
(define-alien-type io-uring-zcrx-ifq-reg
    (struct io-uring-zcrx-ifq-reg
      (if-idx unsigned-int)
      (if-rxq unsigned-int)
      (rq-entries unsigned-int)
      (flags unsigned-int)
      (area-ptr unsigned-long)
      (region-ptr unsigned-long)))

(define-alien-type io-uring-zcrx-rqe
    (struct io-uring-zcrx-rqe
      (off unsigned-long)
      (len unsigned-int)
      (__pad unsigned-int)))

(define-alien-type io-uring-zcrx-cqe
    (struct io-uring-zcrx-cqe
      (off unsigned-long)
      (__pad unsigned-long)))

(define-alien-type io-uring-zcrx-rq
    (struct io-uring-zcrx-rq
      (khead (* unsigned-int))
      (ktail (* unsigned-int))
      (rq-tail unsigned-int)
      (ring-entries unsigned)
      (rqes (* io-uring-zcrx-rqe))
      (ring-ptr (* t))))

;; (defun io-uring-write-once (var val))
;; (defun io-uring-read-once (var))
;; (defun io-uring-smp-store-release (p v))
;; (defun io-uring-smp-load-acquire (p))
;; (defun io-uring-smp-mb ())

(defalien-int io-uring-major-version)
(defalien-int io-uring-minor-version)
(defar io-uring-check-version boolean (major int) (minor int))

(defar io-uring-get-probe-ring (* io-uring-probe) (ring (* io-uring)))
(defar io-uring-get-probe (* io-uring-probe))
(defar io-uring-free-probe void (p (* (struct io-uring-probe))))

;; (defalien-int io-uring-opcode-supported (p (* (struct io-uring-probe))) (op int))
(defalien-int io-uring-queue-init-mem
  (entries unsigned)
  (ring (* io-uring))
  (p (* (struct io-uring-params)))
  (buf (* t)) (buf-size size-t))
(defalien-int io-uring-queue-init-params
  (entries unsigned)
  (ring (* io-uring))
  (p (* (struct io-uring-params))))
(defalien-int io-uring-queue-init (entries int) (ring (* io-uring)) (flags unsigned))
(defalien-int io-uring-queue-mmap (fd int) (p (* (struct io-uring-params))) (ring (* io-uring)))
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
(def-with-ring io-uring-register-ifq
  (reg (* io-uring-zcrx-ifq-reg)))

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
(def-with-ring io-uring-enable-rings)
(def-with-ring ("__io-uring-sqring-wait" io-uring-sqring-wait))
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
(defar io-uring-register-sync-msg int (sqe (* io-uring-sqe)))
(def-with-ring io-uring-register-file-alloc-range (off unsigned) (len unsigned))
(def-with-ring io-uring-register-napi (napi (* io-uring-napi)))
(def-with-ring io-uring-unregister-napi (napi (* io-uring-napi)))
;; ...

(def-with-ring io-uring-get-events)
(def-with-ring io-uring-submit-and-get-events)

;;; Syscalls
(defsyscall io-uring-enter int
  (fd int)
  (to-submit unsigned-int)
  (min-complete unsigned-int)
  (flags unsigned-int)
  (sig (* (struct sigset-t))))

(defsyscall io-uring-enter2 int
  (fd int)
  (to-submit unsigned-int)
  (min-complete unsigned-int)
  (flags unsigned-int)
  (sig (* (struct sigset-t)))
  (sz size-t))
(defsyscall io-uring-setup int
  (entries unsigned-int)
  (p (* (struct io-uring-params))))
(defsyscall io-uring-register int (fd unsigned-int) (opcode unsigned-int) (arg (* t)) (nr-args unsigned-int))

(defsyscall io-uring-setup-buf-ring (* (struct io-uring-buf-ring))
  (ring (* io-uring))
  (nentries unsigned-int)
  (bgid int)
  (flags unsigned-int)
  (ret (* int)))
(defsyscall io-uring-free-buf-ring int
  (ring (* io-uring))
  (br (* (struct io-uring-buf-ring)))
  (nentries unsigned-int)
  (bgid int))

;; __io-uring-get-cqe
(def-with-ring io-uring-set-iowait (enable-iowait boolean))

;;..
(defar io-uring-mlock-size ssize-t (entries unsigned) (flags unsigned))
(defar io-uring-mlock-size-params ssize-t (entries unsigned) (p (* (struct io-uring-params))))
(defar io-uring-memory-size ssize-t (entries unsigned) (flags unsigned))
(defar io-uring-memory-size-params ssize-t (entries unsigned) (p (* (struct io-uring-params))))

