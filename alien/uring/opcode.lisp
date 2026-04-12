;;; uring/opcode.lisp --- Opcodes

;; Wrapper for opcodes defined in liburing/io_uring.h. 

;;; Commentary:

;; 

;;; Code:
(in-package :uring)

(defun opcode (name)
  "Convert a struct name to an opcode as an integer."
  (symbol-value (find-symbol (format nil "+IO-~A+" (caddr (split-sequence #\- (symbol-name name) :count 3))))))

(define-io-op 0 nop ()
  (setf (slot sqe 'fd) -1))

;; preadv2(2)
(def-io-op 1 readv
    ((fd -1 :type file-descriptor)
     (iovecs nil :type (or null system-area-pointer))
     (len 0)
     (ioprio 0 :type (unsigned-byte 16))
     (offset 0 :type (unsigned-byte 64))
     (rw-flags 0)
     (buf-group 0))
  ;; split data into iovecs?
  ;; (setf slot s 'iovecs) iovecs)
  (io-uring-prep-readv sqe fd iovecs len offset))

;; pwritev2(2)
(def-io-op 2 writev
    ((fd -1 :type file-descriptor)
     (iovecs nil :type (or null system-area-pointer))
     (len 0)
     (offset 0 :type (unsigned-byte 64)))
  (io-uring-prep-writev sqe fd iovecs len offset))

;; fsync(2)     
(def-io-op 3 fsync
    ((fd -1 :type file-descriptor)
     (flags 0 :type fixnum))
  (io-uring-prep-fsync sqe fd flags))

;; read from pre-registered buffers
(def-io-op 4 read-fixed
    ((fd -1 :type file-descriptor)
     (buf nil :type (or null system-area-pointer))
     (len 0 :type (unsigned-byte 32))
     (buf-index 0)
     (offset 0))
  (io-uring-prep-read-fixed sqe fd buf len offset buf-index))

(def-io-op 5 write-fixed
    ((fd -1 :type file-descriptor)
     (buf #() :type octet-vector)
     (len 0 :type (unsigned-byte 32))
     (buf-index 0 :type (unsigned-byte 16))
     (offset 0 :type (unsigned-byte 64)))
  (io-uring-prep-write-fixed sqe fd buf len offset buf-index))

;; poll the specified fd
(def-io-op 6 poll-add
    ((fd -1 :type file-descriptor)
     (mask 0))
  (io-uring-prep-poll-add sqe fd mask))

(def-io-op 7 poll-remove
    ((user-data))
  (io-uring-prep-poll-remove sqe user-data))

(def-io-op 8 sync-file-range
    ((fd -1 :type file-descriptor)
     (len 0)
     (offset 0)
     (flags 0))
  (io-uring-prep-sync-file-range sqe fd len offset flags))

(def-io-op 9 sendmsg (fd msg flags) (io-uring-prep-sendmsg sqe fd msg flags))
(def-io-op 10 recvmsg (fd msg flags) (io-uring-prep-recvmsg sqe fd msg flags))
(def-io-op 11 timeout (ts count flags) (io-uring-prep-timeout sqe ts count flags))
(def-io-op 12 timeout-remove (user-data flags) (io-uring-prep-timeout-remove sqe user-data flags))
(def-io-op 13 accept (fd addr addrlen flags) (io-uring-prep-accept sqe fd addr addrlen flags))
(def-io-op 14 async-cancel (user-data flags) (io-uring-prep-cancel64 sqe user-data flags))
(def-io-op 15 link-timeout (ts flags) (io-uring-prep-link-timeout sqe ts flags))
(def-io-op 16 connect (fd addr addrlen) (io-uring-prep-connect sqe fd addr addrlen))
(def-io-op 17 fallocate (fd mode offset len) (io-uring-prep-fallocate sqe fd mode offset len))
(def-io-op 18 openat (dfd path flags mode) (io-uring-prep-openat sqe dfd path flags mode))
(def-io-op 19 close (fd) (io-uring-prep-close sqe fd))
(def-io-op 20 files-update (fds nr-fds offset) (io-uring-prep-files-update sqe fds nr-fds offset))
(def-io-op 21 statx (dfd path flags mask statxbuf) (io-uring-prep-statx sqe dfd path flags mask statxbuf))
(def-io-op 22 read (fd buf nbytes offset) (io-uring-prep-read sqe fd buf nbytes offset))
(def-io-op 23 write (fd buf nbytes offset) (io-uring-prep-write sqe fd buf nbytes offset))
(def-io-op 24 fadvise (fd offset len advice) (io-uring-prep-fadvise sqe fd offset len advice))
(def-io-op 25 madvise (addr len advice) (io-uring-prep-madvise sqe addr len advice))
(def-io-op 26 send (sockfd buf len flags) (io-uring-prep-send sqe sockfd buf len flags))
(def-io-op 27 recv (sockfd buf len flags) (io-uring-prep-recv sqe sockfd buf len flags))
(def-io-op 28 openat2 (dfd path how) (io-uring-prep-openat2 sqe dfd path how))
(def-io-op 29 epoll-ctl (epfd fd op ev) (io-uring-prep-epoll-ctl sqe epfd fd op ev))
(def-io-op 30 splice (fd-in off-in fd-out off-out nbytes splice-flags) (io-uring-prep-splice sqe fd-in off-in fd-out off-out nbytes splice-flags))
(def-io-op 31 provide-buffers (addr len nr bgid bid) (io-uring-prep-provide-buffers sqe addr len nr bgid bid))
(def-io-op 32 remove-buffers (nr bgid) (io-uring-prep-remove-buffers sqe nr bgid))
(def-io-op 33 tee (fd-in fd-out nbytes splice-flags) (io-uring-prep-tee sqe fd-in fd-out nbytes splice-flags))
(def-io-op 34 shutdown (fd how) (io-uring-prep-shutdown sqe fd how))
(def-io-op 35 renameat (olddfd oldpath newdfd newpath flags) (io-uring-prep-renameat sqe olddfd oldpath newdfd newpath flags))
(def-io-op 36 unlinkat (dfd path flags) (io-uring-prep-unlinkat sqe dfd path flags))
(def-io-op 37 mkdirat (dfd path mode) (io-uring-prep-mkdirat sqe dfd path mode))
(def-io-op 38 symlinkat (target newdirfd linkpath) (io-uring-prep-symlinkat sqe target newdirfd linkpath))
(def-io-op 39 linkat (olddfd oldpath newdfd newpath flags) (io-uring-prep-linkat sqe olddfd oldpath newdfd newpath flags))
(def-io-op 40 msg-ring (fd len data flags) (io-uring-prep-msg-ring sqe fd len data flags))
(def-io-op 41 fsetxattr (fd name value flags len) (io-uring-prep-fsetxattr sqe fd name value flags len))
(def-io-op 42 setxattr (name value path flags len) (io-uring-prep-setxattr sqe name value path flags len))
(def-io-op 43 fgetxattr (fd name value len) (io-uring-prep-fgetxattr sqe fd name value len))
(def-io-op 44 getxattr (name value path len) (io-uring-prep-getxattr sqe name value path len))
(def-io-op 45 socket (domain type protocol flags) (io-uring-prep-socket sqe domain type protocol flags))
(def-io-op 46 uring-cmd (cmd-op fd) (io-uring-prep-uring-cmd sqe cmd-op fd))
(def-io-op 47 send-zc (sockfd buf len flags zc-flags) (io-uring-prep-send-zc sqe sockfd buf len flags zc-flags))
(def-io-op 48 sendmsg-zc (fd msg flags) (io-uring-prep-sendmsg-zc sqe fd msg flags))
(def-io-op 49 read-multishot (fd nbytes offset buf-group) (io-uring-prep-read-multishot sqe fd nbytes offset buf-group))
(def-io-op 50 waitid (idtype id infop options flags) (io-uring-prep-waitid sqe idtype id infop options flags))
(def-io-op 51 futex-wait (futex val mask futex-flags flags) (io-uring-prep-futex-wait sqe futex val mask futex-flags flags))
(def-io-op 52 futex-wake (futex val mask futex-flags flags) (io-uring-prep-futex-wake sqe futex val mask futex-flags flags))
(def-io-op 53 futex-waitv (futex nr-futex flags) (io-uring-prep-futex-waitv sqe futex nr-futex flags))
(def-io-op 54 fixed-fd-install (fd flags) (io-uring-prep-fixed-fd-install sqe fd flags))
(def-io-op 55 ftruncate (fd len) (io-uring-prep-ftruncate sqe fd len))
(def-io-op 56 bind (fd addr addrlen) (io-uring-prep-bind sqe fd addr addrlen))
(def-io-op 57 listen (fd backlog) (io-uring-prep-listen sqe fd backlog))
#+todo (def-io-op 58 recv-zc () (io-uring-prep-recv-zc ...))
(def-io-op 59 epoll-wait (fd events maxevents flags) (io-uring-prep-epoll-wait sqe fd events maxevents flags))
(def-io-op 60 readv-fixed
    ((fd -1 :type file-descriptor)
     (iovecs #() :type (array octet-vector))
     (len 0 :type (unsigned-byte 32))
     (buf-index 0)
     (offset 0)
     (ioprio 0 :type (unsigned-byte 16))
     (rw-flags 0))
  (with-slots (fd iovecs len buf-index offset rw-flags) self
    (io-uring-prep-readv-fixed sqe fd iovecs len offset rw-flags buf-index)))
(def-io-op 61 writev-fixed
    ((fd -1 :type file-descriptor)
     (iovecs #() :type (array octet-vector))
     (len 0 :type (unsigned-byte 32))
     (buf-index 0 :type (unsigned-byte 16))
     (ioprio 0 :type (unsigned-byte 16))
     (offset 0 :type (unsigned-byte 64))
     (flags 0 :type fixnum))
  (with-slots (fd iovecs len buf-index ioprio offset flags) self
    (io-uring-prep-writev-fixed sqe fd iovecs len offset flags buf-index)))
(def-io-op 62 pipe (fds pipe-flags) (io-uring-prep-pipe sqe fds pipe-flags))
(def-io-op 63 nop128 () (io-uring-prep-nop128 sqe))
(def-io-op 64 uring-cmd128 (cmd-op fd) (io-uring-prep-uring-cmd128 sqe cmd-op fd))

;; (def-io-op 65 last nil)

(defun opcode-supported-p (op &optional probe)
  (declare (type octet op))
  (let ((p (or probe (io-uring-get-probe))))
    (if (> op (io-uring-probe-last-op p))
        nil
        (/= 0
            (logand
             (io-uring-probe-op-flags (addr (deref (io-uring-probe-ops p) op)))
             io-uring-op-supported)))))
