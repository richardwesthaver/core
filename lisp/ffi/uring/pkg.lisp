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
(defpackage :uring
  (:use :cl :std :sb-alien)
  (:export :load-uring))

(in-package :uring)
(define-alien-loader "uring" t "/usr/lib/")

;;; barrier.h
;; (defun io-uring-write-once (var val))
;; (defun io-uring-read-once (var))
;; (defun io-uring-smp-store-release (p v))
;; (defun io-uring-smp-load-acquire (p))
;; (defun io-uring-smp-mb ())

;;; liburing.h
(defmacro defalien-int (name &rest args)
  `(define-alien-routine ,name int ,@args))

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
(define-alien-routine io-uring-free-probe void (* io-uring-probe))
;;...

(defalien-int io-uring-queue-init (entries int) (ring (* (struct io-uring))) (flags unsigned))


;;...

(defalien-int io-uring-submit (ring (* (struct io-uring))))

(defalien-int io-uring-register
  (fd int)
  (opcode unsigned-int)
  (args (* t))
  (nr-args unsigned-int))

;;...
(defalien-int io-uring-register-buffers
  (ring (* (struct io-uring)))
  (iovecs (* (struct iovec)))
  (nr-iovecs unsigned-int))

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

(defalien-int io-uring-major-version)
(defalien-int io-uring-minor-version)
(defalien-int io-uring-check-version (major int) (minor int))
