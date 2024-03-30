;;; uring/prim.lisp --- URING primitives

;; Primitive functions related to IO_URING.

;;; Commentary:

;; These functions operate directly on foreign-allocated types. You
;; can find all of these in liburing.h. The IOURINGINLINE macro
;; declares relevant functions as both static and inline. Functions
;; declarations prefixed by this macro are re-implemented in Lisp
;; here.

;;; Code:
(in-package :uring)

;; io-uring-opcode-supported-p

;; (sb-alien::sap-int (alien-sap (slot (slot *r1* 'sq) 'khead)))
(defun io-uring-get-sqe (ring)
  (let* ((sq (addr (slot ring 'sq)))
         (head 0)
         (next (1+ (slot sq 'sqe-tail)))
         (shift 0))
    (when (= 1 (logand (slot ring 'flags) ioring-setup-sqe128))
      (setf shift 1))
    (if (/= 1 (logand (slot ring 'flags) ioring-setup-sqpoll))
        ;; IO_URING_READ_ONCE
        (setf head (deref (slot sq 'khead)))
        (setf head (slot sq 'khead)))
    (when (<= (- next head) (slot sq 'ring-entries))
      (prog1
          (addr (deref (slot sq 'sqes) (* (alien-size io-uring-sqe) (ash (logand (slot sq 'sqe-tail) (slot sq 'ring-mask)) shift))))
        (setf (slot (deref sq) 'sqe-tail) next)
        (print (cons head next))))))

;; io-uring-cqe-shift
;; io-uring-cqe-index
;; io-uring-for-each-cqe
;; (defun io-uring-cq-advance (ring nr))
;; io-uring-cqe-seen
;; io-uring-sqe-set-data
;; io-uring-sqe-get-data
;; io-uring-sqe-set-data64
;; io-uring-sqe-get-data64
(defun io-uring-sqe-set-flags (sqe flags)
  (setf (slot sqe 'flags) flags))

(defun io-uring-prep-rw (op sqe fd addr len offset)
  (setf (slot sqe 'opcode) op
        (slot sqe 'flags) 0
        (slot sqe 'ioprio) 0
        (slot sqe 'fd) fd
        (slot sqe 'off-addr-cmd) offset
        (slot sqe 'addr-or-splice-off-in) addr
        (slot sqe 'len) len
        (slot sqe 'flags2) (deref (make-alien (union io-uring-sqe-slot8)))
        (slot sqe 'buf-opt) (deref (make-alien (union io-uring-sqe-slot10)))
        (slot sqe 'personality) 0
        (slot sqe 'splice-index-addr) (deref (make-alien (union io-uring-sqe-slot12)))
        (slot sqe 'addr-or-cmd) (deref (make-alien (union io-uring-sqe-slot13))))
  sqe)

;; io-uring-prep-splice
;; (with-io-uring (ring)
;;   (io-uring-queue-init 16 ring 1)
;;   (io-uring-get-sqe ring))
