;;; uring/prim.lisp --- URING primitives

;; Primitive functions related to IO_URING.

;;; Commentary:

;; These functions operate directly on foreign-allocated types. You
;; can find all of these in liburing.h. The IOURINGINLINE macro
;; declares relevant functions as both static and inline. Function
;; declarations prefixed by this macro are re-implemented in Lisp
;; here.

;;; Code:
(in-package :uring)

(defun io-uring-opcode-supported-p (probe op)
  (if (> op (slot probe 'last-op)) 
      0
      (not (zerop (logand (slot (deref (slot probe 'ops) op) 'flags) io-uring-op-supported)))))

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

#+todo
(defmacro io-uring-for-each-cqe (ring head cqe))

(defun io-uring-cq-advance (ring nr)
  (when (< 0 nr)
    (let* ((cq (addr (slot ring 'cq)))
          (head (slot cq 'khead)))
      ;; smp-store-release
      (setf head (+ nr (deref head))))))

(defun io-uring-cqe-seen (ring cqe)
  (unless (null-alien cqe)
    (io-uring-cq-advance ring 1)))

(defun io-uring-sqe-set-data (sqe data) ;; the C function returns (* void)
  (setf (slot sqe 'user-data) data))

(defun io-uring-cqe-get-data (cqe)
  (slot cqe 'user-data))

(defun io-uring-sqe-set-data64 (sqe data)
  "Assign a 64-bit value to this sqe which can be retrieved with
io-uring-cqe-get-data64 instead of a pointer."
  (declare (type (unsigned-byte 64) data))
  (setf (slot sqe 'user-data) data))

(defun io-uring-cqe-get-data64 (cqe)
  "Same as IO-URING-CQE-GET-DATA but return value is (unsigned-byte 64) value
instead of a pointer."
  (slot cqe 'user-data))

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
        (slot sqe 'flags2) (deref (make-alien io-uring-sqe-slot8))
        (slot sqe 'buf-opt) (deref (make-alien io-uring-sqe-slot10))
        (slot sqe 'personality) 0
        (slot sqe 'splice-index-addr) (deref (make-alien io-uring-sqe-slot12))
        (slot sqe 'addr-or-cmd) (deref (make-alien io-uring-sqe-slot13)))
  sqe)

(defun io-uring-prep-splice (sqe fd-in off-in fd-out off-out nbytes splice-flags)
  (io-uring-prep-rw +io-splice+ sqe fd-out nil nbytes off-out)
  (setf (slot sqe 'splice-off-in) off-in
        (slot sqe 'splice-fd-in) fd-in
        (slot sqe 'splice-flags) splice-flags))

(defun io-uring-prep-tee (sqe fd-in fd-out nbytes splice-flags)
  (io-uring-prep-rw +io-tee+ sqe fd-out nil nbytes 0)
  (setf (slot sqe 'splice-off-in) 0)
  (setf (slot sqe 'splice-fd-in) fd-in)
  (setf (slot sqe 'splice-flags) splice-flags))

(defun io-uring-prep-readv (sqe fd iovecs nr-vecs offset)
  (io-uring-prep-rw +io-readv+ sqe fd iovecs nr-vecs offset))

(defun io-uring-prep-readv2 (sqe fd iovecs nr-vecs offset flags)
  (io-uring-prep-rw +io-readv+ sqe fd iovecs nr-vecs offset)
  (setf (slot sqe 'rw-flags) flags))

(defun io-uring-prep-read-fixed (sqe fd buf nbytes offset buf-index)
  (io-uring-prep-rw +io-read-fixed+ sqe fd buf nbytes offset)
  (setf (slot sqe 'buf-index) buf-index))

(defun io-uring-prep-readv-fixed (sqe fd iovecs nr-vecs offset flags buf-index)
  (io-uring-prep-readv2 sqe fd iovecs nr-vecs offset flags)
  (setf (slot sqe 'opcode) +io-readv-fixed+
        (slot sqe 'buf-index) buf-index))

(defun io-uring-prep-writev (sqe fd iovecs nr-vecs offset)
  (io-uring-prep-rw +io-writev+ sqe fd iovecs nr-vecs offset))

(defun io-uring-prep-writev2 (sqe fd iovecs nr-vecs offset flags)
  (io-uring-prep-writev sqe fd iovecs nr-vecs offset)
  (setf (slot sqe 'rw-flags) flags))

(defun io-uring-prep-write-fixed (sqe fd buf nbytes offset buf-index)
  (io-uring-prep-rw +io-write-fixed+ sqe fd buf nbytes offset)
  (setf (slot sqe 'buf-index) buf-index))

(defun io-uring-prep-writev-fixed (sqe fd iovecs nr-vecs offset flags buf-index)
  (io-uring-prep-writev2 sqe fd iovecs nr-vecs offset flags)
  (setf (slot sqe 'opcode) +io-writev-fixed+
        (slot sqe 'buf-index) buf-index))

(defun io-uring-prep-recvmsg (sqe fd msg flags)
  (io-uring-prep-rw +io-recvmsg+ sqe fd msg 1 0)
  (setf (slot sqe 'msg-flags) flags))

(defun io-uring-prep-recvmsg-multishot (sqe fd msg flags)
  (io-uring-prep-recvmsg sqe fd msg flags)
  ;; REVIEW 2026-04-07: 
  (setf (slot sqe 'ioprio) (logior (slot sqe 'ioprio) flags)))


;; ...

(defun io-uring-prep-nop (sqe)
  (io-uring-prep-rw +io-nop+ sqe -1 nil 0 0))

;; (with-io-uring (ring)
;;   (io-uring-queue-init 160 ring 1)
;;   (io-uring-get-sqe ring))

(definline io-uring-load-sq-head (ring)
  (if (< 0 (logand (slot ring 'flags) ioring-setup-sqpoll))
      (barrier (:write) (deref (slot (slot ring 'sq) 'khead)))
      (deref (slot (slot ring 'sq) 'khead))))

(definline io-uring-sq-ready (ring)
  (- (slot (slot ring 'sq) 'sqe-tail) (io-uring-load-sq-head ring)))

(definline io-uring-sq-space-left (ring)
  (- (slot (slot ring 'sq) 'ring-entries) (io-uring-sq-ready ring)))

(definline io-uring-sqe-shift-from-flags (flags)
  (lognot (lognot (logand flags ioring-setup-sqe128))))

(definline io-uring-sqe-shift (ring)
  (io-uring-sqe-shift-from-flags (slot ring 'flags)))

;; ...

(definline io-uring-buf-ring-mask (ring-entries)
  (1- ring-entries))

(definline io-uring-buf-ring-init (br)
  (setf (slot br 'tail) 0))

;; ...
