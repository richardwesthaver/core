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
  (:use :cl :std :sb-alien :dat/proto)
  (:export :load-uring :io-uring-cq :io-uring-cq*
   :completion-queue-offsets :completion-queue
   :completion-queue-entry :completion-queue-entry-32
   :io-uring-sq :io-uring-sq* :submission-queue-offsets :submission-queue 
   :submission-queue-entry :submission-queue-entry-128
   :io-memory-map :parse-io-uring-params :io-params :io-uring
   :uring :*default-io-params* :uring-builder :setup-queue
   :make-queue :build-submitter :sigset-t :cpu-set-t
   :cpu-mask-t :recv-msg-out :cancel-builder :mmapped-region))
   

(in-package :uring)
(define-alien-loader "uring" t "/usr/lib/")
