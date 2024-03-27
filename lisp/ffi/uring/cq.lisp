;;; uring/cq.lisp --- Completion Queue

;; 

;;; Code:
(in-package :uring)

;; Raw completion-queue
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

(defstruct completion-queue
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (queue nil :type io-uring-cq*))

;; (define-alien-type io-uring-cqe* (* (struct io-uring-cqe)))

;; 16-byte CQE
(defstruct completion-queue-entry (entry nil :type io-uring-cqe))
;; 32-byte CQE
(defstruct completion-queue-entry-32 (entry nil :type io-uring-cqe)
           (ext #(0 0) :type (array fixnum 2)))

;; sync, fill, pop
;; check-overflow
;; eventfd support?
