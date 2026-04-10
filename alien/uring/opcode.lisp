;;; uring/opcode.lisp --- Opcodes

;; Wrapper for opcodes defined in liburing/io_uring.h. 

;;; Commentary:

;; 

;;; Code:
(in-package :uring)

(defun opcode (name)
  "Convert a struct name to an opcode as an integer."
  (symbol-value (find-symbol (format nil "+IO-~A+" (caddr (split-sequence #\- (symbol-name name) :count 3))))))

(def-io-op 0 nop ()
  (setf (slot sqe 'fd) -1))

;; preadv2(2)
(def-io-op 1 readv
    ((fd -1 :type file-descriptor)
     (iovecs #() :type (array octet-vector))
     (len 0 :type fixnum)
     (ioprio 0 :type (unsigned-byte 16))
     (offset 0 :type (unsigned-byte 64))
     (rw-flags 0)
     (buf-group 0))
  ;; split data into iovecs?
  ;; (setf slot s 'iovecs) iovecs)
  (with-slots (fd iovecs len offset) self
    (io-uring-prep-readv sqe fd iovecs len offset)))

;; pwritev2(2)
(def-io-op 2 writev
    ((fd -1 :type file-descriptor)
     (iovec #() :type (array octet-vector))
     (len 0 :type fixnum)
     (offset 0 :type (unsigned-byte 64)))
  (with-slots (fd iovecs len offset) self
    (io-uring-prep-writev sqe fd iovecs len offset)))

;; fsync(2)     
(def-io-op 3 fsync
    ((fd -1 :type file-descriptor)
     (flags 0 :type fixnum))
  (with-slots (fd flags) self
    (io-uring-prep-fsync sqe fd flags)))

;; read from pre-registered buffers
(def-io-op 4 read-fixed
    ((fd -1 :type file-descriptor)
     (buf #() :type octet-vector)
     (len 0 :type (unsigned-byte 32))
     (buf-index 0)
     (offset 0))
  (with-slots (fd buf len buf-index offset) self
    (io-uring-prep-read-fixed sqe fd buf len offset buf-index)))

(def-io-op 5 write-fixed
    ((fd -1 :type file-descriptor)
     (buf #() :type octet-vector)
     (len 0 :type (unsigned-byte 32))
     (buf-index 0 :type (unsigned-byte 16))
     (offset 0 :type (unsigned-byte 64)))
  (with-slots (fd buf len buf-index offset) self
    (io-uring-prep-write-fixed sqe fd buf len offset buf-index)))

;; poll the specified fd
(def-io-op 6 poll-add
    ((fd -1 :type file-descriptor)
     (mask 0))
  (with-slots (fd mask) self
    (io-uring-prep-poll-add sqe fd mask)))

(def-io-op 7 poll-remove
    ((user-data))
  (with-slots (user-data) self
    (io-uring-prep-poll-remove sqe user-data)))

(def-io-op 8 sync-file-range
    ((fd -1 :type file-descriptor)
     (len 0)
     (offset 0)
     (flags 0))
  (with-slots (fd len offset flags) self
    (io-uring-prep-sync-file-range sqe fd len offset flags)))

(def-io-op 9 sendmsg nil)
(def-io-op 10 recvmsg nil)
(def-io-op 11 timeout nil)
(def-io-op 12 timeout-remove nil)
(def-io-op 13 accept nil)
(def-io-op 14 async-cancel nil)
(def-io-op 15 link-timeout nil)
(def-io-op 16 connect nil)
(def-io-op 17 fallocate nil)
(def-io-op 18 openat nil)
(def-io-op 19 close nil)
(def-io-op 20 files-update nil)
(def-io-op 21 statx nil)
(def-io-op 22 read nil)
(def-io-op 23 write nil)
(def-io-op 24 fadvise nil)
(def-io-op 25 madvise nil)
(def-io-op 26 send nil)
(def-io-op 27 recv nil)
(def-io-op 28 openat2 nil)
(def-io-op 29 epoll-ctl nil)
(def-io-op 30 splice nil)
(def-io-op 31 provide-buffers nil)
(def-io-op 32 remove-buffers nil)
(def-io-op 33 tee nil)
(def-io-op 34 shutdown nil)
(def-io-op 35 renameat nil)
(def-io-op 36 unlinkat nil)
(def-io-op 37 mkdirat nil)
(def-io-op 38 symlinkat nil)
(def-io-op 39 msg-ring nil)
(def-io-op 40 fsetxattr nil)
(def-io-op 41 setxattr nil)
(def-io-op 42 fgetxattr nil)
(def-io-op 43 getxattr nil)
(def-io-op 44 socket nil)
(def-io-op 45 uring-cmd nil)
(def-io-op 46 send-zc nil)
(def-io-op 47 sendmsg-zc nil)
(def-io-op 48 read-multishot nil)
(def-io-op 49 waitid nil)
(def-io-op 50 futex-wait nil)
(def-io-op 51 futex-wake nil)
(def-io-op 52 futex-waitv nil)
(def-io-op 53 fixed-fd-install nil)
(def-io-op 54 ftruncate nil)
(def-io-op 55 bind nil)
(def-io-op 56 listen nil)
(def-io-op 57 recv-zc nil)
(def-io-op 58 epoll-wait nil)
(def-io-op 59 readv-fixed
    ((fd -1 :type file-descriptor)
     (iovecs #() :type (array octet-vector))
     (len 0 :type (unsigned-byte 32))
     (buf-index 0)
     (offset 0)
     (ioprio 0 :type (unsigned-byte 16))
     (rw-flags 0))
  (with-slots (fd iovecs len buf-index offset rw-flags) self
    (io-uring-prep-readv-fixed sqe fd iovecs len offset rw-flags buf-index)))
(def-io-op 60 writev-fixed
    ((fd -1 :type file-descriptor)
     (iovecs #() :type (array octet-vector))
     (len 0 :type (unsigned-byte 32))
     (buf-index 0 :type (unsigned-byte 16))
     (ioprio 0 :type (unsigned-byte 16))
     (offset 0 :type (unsigned-byte 64))
     (flags 0 :type fixnum))
  (with-slots (fd iovecs len buf-index ioprio offset flags) self
    (io-uring-prep-writev-fixed sqe fd iovecs len offset flags buf-index)))
(def-io-op 61 pipe nil)
(def-io-op 62 nop128 nil)
(def-io-op 63 uring-cmd128 nil)

;; (def-io-op 64 last nil)

(defun opcode-supported-p (op &optional probe)
  (declare (type octet op))
  (let ((p (or probe (io-uring-get-probe))))
    (if (> op (io-uring-probe-last-op p))
        nil
        (/= 0
            (logand
             (io-uring-probe-op-flags (addr (deref (io-uring-probe-ops p) op)))
             io-uring-op-supported)))))
