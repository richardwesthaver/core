;;; uring/pkg.lisp --- URING Systems

;; /usr/include/liburing.h

;;; Commentary:

;; IO_URING is our preferred means of IO on Linux. The bindings here
;; are used by the high-level library IO.

;; As a point of reference, we look to the SB-SYS:SERVE-EVENT function
;; in SBCL. This is an async event loop which dispatches to a backend
;; based on features. On Linux it will use either poll or select(2),
;; neither of which are particularly fast.

;; Using the bindings provided by this library we will implement an
;; alternative backend to dispatch to.

;; ref: https://kernel.dk/io_uring.pdf

;; guide: https://unixism.net/loti/low_level.html

;; tokio/io-uring: https://github.com/tokio-rs/io-uring

#|

There are two fundamental operations associated with an async
interface: the act of submitting a request, and the event that is
associated with the completion of said request.

For submitting IO, the application is the producer and the kernel is
the consumer. The opposite is true for completions - here the kernel
produces completion events and the application consumes them.

Hence, we need a pair of rings to provide an effective communication
channel between an application and the kernel. That pair of rings is
at the core of the new interface, io_uring.

They are suitably named submission queue (SQ), and completion
queue (CQ), and form the foundation of the new interface.

|#
;;; Code:
(defpkg :uring
  (:use :cl :std :sb-alien :sys)
  (:import-from :sb-posix :file-descriptor :sap-or-nil)
  (:export :load-uring :io-uring-cq
           :completion-queue-offsets :completion-queue
           :completion-queue-entry :completion-queue-entry-32
           :io-uring-sq :submission-queue-offsets :submission-queue
   :io-uring-cqe :io-uring-sqe
   :submission-queue-entry :submission-queue-entry-128
   :io-memory-map :parse-io-uring-params :io-params :io-uring
   :with-io-uring :with-new-io-uring :io-uring-get-sqe :io-uring-sqe-set-flags
   :with-io-sqe :with-new-io-sqe :with-io-cqe :with-new-io-cqe
   :io-restriction-p :*default-io-params* :*default-io-entry-count* :io-submitter
   :make-io-submitter :make-uring-queue :sigset :cpu-set
   :cpu-mask-t :recv-msg-out :cancel-builder :*default-io-params* 
   :io-uring-prep-rw :make-io-restriction :io-restriction :uring-builder 
   :setup-uring-queue))

(in-package :uring)
(define-alien-loader uring "/usr/lib/")
;; defaults on x86_64
(defconstant +cpu-setsize+ 16) 
(defconstant +sigset-nwords+ 16)
(defconstant +ncpu-bits+ 16)

(define-alien-type kernel-rwf-t int)

(define-alien-type io-uring-op unsigned-int)

(define-alien-type io-uring-restriction-slot2
    (union io-uring-restriction-slot2
      (register-op unsigned-char)
      (sqe-op unsigned-char)
      (sqe-flags unsigned-char)))

(define-alien-type io-uring-restriction
    (struct io-uring-restriction
      (opcode unsigned-short)
      (op-or-flags (union io-uring-restriction-slot2))
      (resv unsigned-char)
      (resv2 (array unsigned-int 3))))

(define-alien-type io-uring-buf-ring-resv-and-tail
    (struct io-uring-buf-ring-resv-and-tail
      (resv1 unsigned-long)
      (resv2 unsigned-int)
      (resv3 unsigned-short)
      (tail unsigned-short)))

(define-alien-type io-uring-buf-ring-slot1
    (union io-uring-buf-ring-slot1
      (resv-and-tail io-uring-buf-ring-resv-and-tail)
      (bufs (* (struct io-uring-buf)))))

(define-alien-type io-uring-buf-ring
    (struct io-uring-buf-ring
      (tail-or-bufs (union io-uring-buf-ring-slot1))))

(define-alien-type io-uring-sqe-cmd-op-and-pad
    (struct io-uring-sqe-cmd-op-and-pad
      (cmd-op unsigned-int)
      (pad unsigned-int)))

(define-alien-type io-uring-sqe-slot5
    (union io-uring-sqe-slot5
      (off unsigned-long)
      (addr2 unsigned-long)
      (cmd-op-and-pad (struct io-uring-sqe-cmd-op-and-pad))))

(define-alien-type io-uring-sqe-slot6
    (union io-uring-sqe-slot6
      (addr unsigned-long)
      (splice-off-in unsigned-long)))

(define-alien-type io-uring-sqe-slot8
    (union io-uring-sqe-slot8
      (rw-flags kernel-rwf-t)
      (fsync-flags unsigned-int)
      (poll-events unsigned-short)
      (poll32-events unsigned-int)
      (sync-range-flags unsigned-int)
      (msg-flags unsigned-int)
      (timeout-flags unsigned-int)
      (accept-flags unsigned-int)
      (cancel-flags unsigned-int)
      (open-flags unsigned-int)
      (statx-flags unsigned-int)
      (fadvise-advice unsigned-int)
      (splice-flags unsigned-int)
      (rename-flags unsigned-int)
      (unlink-flags unsigned-int)
      (hardlink-flags unsigned-int)
      (xattr-flags unsigned-int)
      (msg-ring-flags unsigned-int)
      (uring-cmd-flags unsigned-int)))

(define-alien-type io-uring-sqe-slot10
    (union io-uring-sqe-slot10
      (buf-index unsigned-short)
      (buf-group unsigned-short)))

(define-alien-type io-uring-sqe-addr-len-and-pad
    (struct io-uring-sqe-addr-len-and-pad
      (addr-len unsigned-short)
      (pad3 (array unsigned-short 1))))

(define-alien-type io-uring-sqe-slot12
    (union io-uring-sqe-slot12
      (splice-fd-in int)
      (file-index unsigned-int)
      (addr-len-and-pad (struct io-uring-sqe-addr-len-and-pad))))

(define-alien-type io-uring-sqe-addr3-and-pad
    (struct io-uring-sqe-addr3-and-pad
      (addr3 (* t))
      (pad2 (array unsigned-long 1))))

(define-alien-type io-uring-sqe-slot13
    (union io-uring-sqe-slot13
      (addr3-and-pad (struct io-uring-sqe-addr3-and-pad))
      (cmd (array unsigned-char 0))))

(define-alien-type io-uring-sqe
    (struct io-uring-sqe
      (opcode unsigned-char)
      (flags unsigned-char)
      (ioprio unsigned-short)
      (fd int)
      ;; (off-addr-cmd io-uring-sqe-slot5)
      (off-addr-cmd unsigned-long)
      ;; (addr-or-splice-off-in io-uring-sqe-slot6)
      (addr-or-splice-off-in (* t))
      (len unsigned-int)
      ;; (flags2 (union io-uring-sqe-slot8))
      (flags2 unsigned-int)
      (user-data (* t))
      ;; (buf-opt (union io-uring-sqe-slot10))
      (buf-opt unsigned-short)
      (personality unsigned-short)
      ;; (splice-index-addr (union io-uring-sqe-slot12))
      (splice-index-addr unsigned-int)
      ;; (addr-or-cmd (union io-uring-sqe-slot13))
      ;; 2x u64
      (addr-or-cmd io-uring-sqe-addr3-and-pad)))

;; NOTE 2024-05-12: alpha and mips use 535,536,537
(defconstant +nr-io-uring-setup+ 425)
(defconstant +nr-io-uring-enter+ 426)
(defconstant +nr-io-uring-register+ 427)

(define-alien-type io-uring-sq
    (struct io-uring-sq
      (khead (* unsigned))
      (ktail (* unsigned))
      (kring-mask (* unsigned)) ;; deprecated
      (kring-entries (* unsigned)) ;; deprecated
      (kflags (* unsigned))
      (kdropped (* unsigned))
      (array (* unsigned))
      (sqes (* (struct io-uring-sqe)))
      (sqe-head unsigned)
      (sqe-tail unsigned)
      (ring-sz sb-unix:size-t)
      (ring-ptr (* t))
      (ring-mask unsigned)
      (ring-entries unsigned)
      (pad (array unsigned 2))))

(define-alien-type io-uring-cq
    (struct io-uring-cq
      (khead (* unsigned))
      (ktail (* unsigned))
      (kring-mask (* unsigned)) ;; deprecated
      (kring-entries (* unsigned)) ;; deprecated
      (kflags (* unsigned))
      (koverflow (* unsigned))
      (cqes (* (struct io-uring-cqe)))
      (ring-sz sb-unix:size-t)
      (ring-ptr (* t))
      (ring-mask unsigned)
      (ring-entries unsigned)
      (pad (array unsigned-int 2))))

(define-alien-type io-uring
    (struct io-uring
      (sq (struct io-uring-sq))
      (cq (struct io-uring-cq))
      (flags unsigned)
      (ring-fd int)
      (features unsigned)
      (enter-ring-fd int)
      (int-flags unsigned-char)
      (pad (array unsigned-char 3))
      (pad2 unsigned)))
