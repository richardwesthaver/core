;;; uring/cq.lisp --- Completion Queue

;; 

;;; Code:
(in-package :uring)

(defstruct completion-queue-offsets
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (ring-mask 0 :type fixnum)
  (ring-entries 0 :type fixnum)
  (overflow 0 :type fixnum)
  (cqes 0 :type fixnum)
  (flags 0 :type fixnum)
  ;; resv1
  (user-addr 0 :type fixnum))

(defmethod build ((self completion-queue-offsets) &key &allow-other-keys)
  (with-slots (head tail ring-mask ring-entries overflow cqes flags user-addr) self
      (with-io-cqring-offsets res
          ((head head) (tail tail) (ring-mask ring-mask) (ring-entries ring-entries)
           (overflow overflow) (cqes cqes) (flags flags) (user-addr user-addr))
        res)))

(defstruct completion-queue
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (queue (make-alien io-uring-cq) :type (alien io-uring-cq*))) ;; io-uring-cq*

;; (define-alien-type io-uring-cqe* (* (struct io-uring-cqe)))

;; 16-byte CQE
(defstruct completion-queue-entry 
  (user-data 0 :type fixnum)
  (res 0 :type fixnum)
  (flags 0 :type fixnum))

(defmethod build ((self completion-queue-entry) &key &allow-other-keys)
  (with-slots (user-data res flags) self
      (with-io-uring-cqe ret
          ((user-data user-data) (res res) (flags flags))
        ret)))

(defmethod build ((self completion-queue-entry) &key &allow-other-keys)
  (build (make-completion-queue-entry-32 :entry self))
  (with-slots (user-data res flags) self
    (with-io-uring-cqe ret
        ((user-data user-data) (res res) (flags flags))
      ret)))

;; 32-byte CQE
(defstruct completion-queue-entry-32
  (entry (make-completion-queue-entry) :type completion-queue-entry))
  ;; big-cqe = 16 bytes of padding u64*2

(defmethod build ((self completion-queue-entry-32) &key &allow-other-keys)
  (with-slots (entry) self
    (with-slots (user-data res flags) entry
      (with-alien ((big-cqe (array unsigned-long 2)))
        ;; TODO this may need to change to align with new version of WITH-IO-URING-SQE
        (with-io-uring-cqe ret
            ((user-data user-data) (res res) (flags flags) (big-cqe big-cqe))
          ret)))))

;; sync, fill, pop
;; check-overflow
;; eventfd support?
