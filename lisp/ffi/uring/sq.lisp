;;; uring/sq.lisp --- Submission Queue

;;

;;; Code:
(in-package :uring)

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

(defstruct submission-queue-offsets
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (ring-mask 0 :type fixnum)
  (ring-entries 0 :type fixnum)
  (flags 0 :type fixnum)
  (dropped 0 :type fixnum)
  (array 0 :type fixnum)
  ;; resv1
  (user-addr 0 :type fixnum))

;; used to send IO requests to the kernel
(defstruct submission-queue
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (queue nil :type io-uring-sq*))

;; 64-byte SQE
(defstruct submission-queue-entry (entry nil :type io-uring-sqe))
;; 128-byte SQE
(defstruct submission-queue-entry-128
  (entry nil :type io-uring-sqe)
  (ext (make-array 64 :element-type 'octet) :type (octet-vector 64)))

;;; Flags

;; sync, needs-wakeup-p, dropped, overflowp, taskrunp, push, push-multiple, push* (unchecked), personality
