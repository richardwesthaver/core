;;; uring/sq.lisp --- Submission Queue

;;

;;; Code:
(in-package :uring)

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

(defmethod build ((self submission-queue-offsets) &key &allow-other-keys)
  (with-slots (head tail ring-mask ring-entries flags dropped array user-addr) self
      (with-io-sqring-offsets res
          ((head head) (tail tail) (ring-mask ring-mask) (ring-entries ring-entries) (flags flags)
           (dropped dropped) (array array) (user-addr user-addr))
        res)))

;; used to send IO requests to the kernel
(defstruct submission-queue
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (queue (make-alien io-uring-sq) :type (alien io-uring-sq*))) ;; io-uring-sq*

;; 64-byte SQE
(defstruct submission-queue-entry
  (opcode 0 :type octet)
  (flags 0 :type octet)
  (ioprio 0 :type (unsigned-byte 16))
  (fd 0 :type sb-posix:file-descriptor)
  (off 0 :type (unsigned-byte 64))
  (addr 0 :type fixnum)
  (len 0 :type fixnum)
  (flags2 0 :type fixnum)
  (user-data 0 :type fixnum)
  (buf-index 0 :type fixnum)
  (personality 0 :type fixnum)
  (file-index 0 :type fixnum)
  (addr2 0 :type (unsigned-byte 64))) ;; this is actually addr3. it's a u64 which is inside a struct, inside a union

(defun u64-bytes (int)
  (make-array 8 :element-type 'octet 
                :initial-contents
                (loop for i from 0 below 8
                      collect (ldb (byte 8 (* i 8)) int))))

(defmethod build ((self submission-queue-entry) &key &allow-other-keys)
  (with-slots (opcode flags ioprio fd off addr len flags2 user-data buf-index personality file-index addr2) self
    (with-alien ((a (array unsigned-char 80)))
      (clone-octets-to-alien (u64-bytes addr2) a)
      ;; TODO
      ;; (with-io-uring-sqe res
      ;;     ((opcode opcode) (flags flags) (ioprio ioprio) (fd fd) (off off) (addr addr) (len len)
      ;;      (flags2 flags2) (user-data user-data) (buf-index buf-index) (personality personality)
      ;;      (file-index file-index)
      ;;      (addr2 a))
      ;;   res)
      )))

;; 128-byte SQE
(defstruct submission-queue-entry-128
  (opcode 0 :type octet)
  (flags 0 :type octet)
  (ioprio 0 :type (unsigned-byte 16))
  (fd 0 :type sb-posix:file-descriptor)
  (off 0 :type (unsigned-byte 64))
  (addr 0 :type fixnum)
  (len 0 :type fixnum)
  (flags2 0 :type fixnum)
  (user-data 0 :type fixnum)
  (buf-index 0 :type fixnum)
  (personality 0 :type fixnum)
  (file-index 0 :type fixnum)
  ;; with sqe128, this field contains the last 72 bytes of the 80 byte
  ;; arbitrary command data field. The first 8 bytes live in the ADDR2
  ;; slot of ENTRY.
  (cmd (make-array 80 :element-type 'octet) :type (octet-vector 80)))

(defmethod build ((self submission-queue-entry-128) &key &allow-other-keys)
  (with-slots (opcode flags ioprio fd off addr len flags2 user-data buf-index personality file-index cmd) self
    (with-alien ((a (array unsigned-char 80)))
      (clone-octets-to-alien cmd a)
      ;; (with-io-uring-sqe res
      ;;     ((opcode opcode) (flags flags) (ioprio ioprio) (fd fd) (off off) (addr addr) (len len)
      ;;      (flags2 flags2) (user-data user-data) (buf-index buf-index) (personality personality)
      ;;      (file-index file-index) (addr2 a))
      ;;   res)
      )))
;;; Flags

;; sync, needs-wakeup-p, dropped, overflowp, taskrunp, push,
;; push-multiple, push* (unchecked), personality
