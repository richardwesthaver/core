;;; uring/prim.lisp --- URING primitives

;; Primitive functions related to IO_URING.

;;; Commentary:

;; These functions operate directly on foreign-allocated types. You can find
;; all of these in liburing.h. The IOURINGINLINE macro declares relevant
;; functions as both static and inline. Function declarations prefixed by this
;; macro are re-implemented in Lisp here.

;;; Code:
(in-package :uring)

(defun io-uring-opcode-supported-p (op &optional ring)
  (let ((probe (if ring (io-uring-get-probe-ring ring) (io-uring-get-probe))))
    (unless (> op (slot probe 'last-op))
      (not (zerop (logand (slot (deref (slot probe 'ops) op) 'flags) io-uring-op-supported))))))

(definline io-uring-cqe-shift-from-flags (flags)
  (lognot (lognot (logand flags ioring-setup-cqe32))))

(definline io-uring-cqe-shift (ring)
  (io-uring-cqe-shift-from-flags (slot ring 'flags)))

(definline io-uring-cqe-nr (cqe)
  (ash 1 (lognot (lognot (logand (slot cqe 'flags) ioring-cqe-f-32)))))

;; io-uring-cqe-iter-init
;; io-uring-cqe-iter-next

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
        (slot sqe 'flags2) 0
        (slot sqe 'buf-opt) 0
        (slot sqe 'personality) 0
        (slot sqe 'splice-index-addr) 0
        (slot sqe 'addr-or-cmd) (sap-alien (foreign-alloc '(struct io-uring-sqe-addr3-and-pad)) io-uring-sqe-addr3-and-pad))
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

(defun io-uring-prep-sendmsg (sqe fd msg flags)
  (io-uring-prep-rw +io-sendmsg+ sqe fd msg 1 0)
  (setf (slot sqe 'msg-flags) flags))

(defun io-uring-prep-poll-add (sqe fd poll-mask)
  (io-uring-prep-rw +io-poll-add+ sqe fd nil 0 0)
  ;; may not work on big-endian (see __io_uring_prep_poll_mask)
  (setf (slot sqe 'poll32-events) poll-mask))

(defun io-uring-prep-poll-multishot (sqe fd poll-mask)
  (io-uring-prep-poll-add sqe fd poll-mask)
  (setf (slot sqe 'len) ioring-poll-add-multi))

(defun io-uring-prep-poll-remove (sqe user-data)
  (io-uring-prep-rw +io-poll-remove+ sqe -1 nil 0 0)
  (setf (slot sqe 'addr) user-data))

(defun io-uring-prep-poll-update (sqe old-user-data new-user-data poll-mask flags)
  (io-uring-prep-rw +io-poll-remove+ sqe -1 nil flags new-user-data)
  (setf (slot sqe 'addr) old-user-data
        ;; may not work on BE
        (slot sqe 'poll32-events) poll-mask))

(defun io-uring-prep-fsync (sqe fd fsync-flags)
  (io-uring-prep-rw +io-fsync+ sqe fd nil 0 0)
  (setf (slot sqe 'fsync-flags) fsync-flags))

(defun io-uring-prep-nop (sqe)
  (io-uring-prep-rw +io-nop+ sqe -1 nil 0 0))

(defun io-uring-prep-nop128 (sqe)
  (io-uring-prep-rw +io-nop128+ sqe -1 nil 0 0))

(defun io-uring-prep-timeout (sqe ts count flags)
  (io-uring-prep-rw +io-timeout+ sqe -1 ts 1 count)
  (setf (slot sqe 'timeout-flags) flags))

(defun io-uring-prep-timeout-update (sqe ts user-data flags)
  (io-uring-prep-rw +io-timeout-remove+ sqe -1 nil 0 ts)
  (setf (slot sqe 'addr) user-data
        (slot sqe 'timeout-flags) (logior flags ioring-timeout-update)))

(defun io-uring-prep-timeout-remove (sqe user-data flags)
  (io-uring-prep-rw +io-timeout-remove+ sqe -1 nil 0 0)
  (setf (slot sqe 'addr) user-data
        (slot sqe 'timeout-flags) flags))

(defun io-uring-prep-accept (sqe fd addr addrlen flags)
  (io-uring-prep-rw +io-accept+ sqe fd addr 0 addrlen)
  (setf (slot sqe 'accept-flags) flags))

(defun %io-uring-set-target-fixed-file (sqe index)
  (setf (slot sqe 'file-index) (1+ index)))

(defun io-uring-initialize-sqe (sqe)
  (declare ((alien (* io-uring-sqe)) sqe))
  (let ((addr (foreign-alloc '(struct io-uring-sqe-addr3-and-pad))))
    (setf (slot sqe 'flags) 0
          (slot sqe 'ioprio) 0
          (slot sqe 'flags2) 0
          (slot sqe 'buf-opt) 0
          (slot sqe 'personality) 0
          (slot sqe 'splice-index-addr) 0
          (slot sqe 'addr-or-cmd) (sap-alien addr (struct io-uring-sqe-addr3-and-pad)))))

(defun io-uring-prep-accept-direct (sqe fd addr addrlen flags file-index)
  (io-uring-prep-accept sqe fd addr addrlen flags)
  ;; offset by 1 for allocation
  (when (= file-index ioring-file-index-alloc) (decf file-index))
  (%io-uring-set-target-fixed-file sqe file-index))

(defun io-uring-prep-multishot-accept (sqe fd addr addrlen flags)
  (io-uring-prep-accept sqe fd addr addrlen flags)
  (setf (slot sqe 'ioprio) (logior (slot sqe 'ioprio) ioring-accept-multishot)))

(defun io-uring-prep-multishot-accept-direct (sqe fd addr addrlen flags)
  (io-uring-prep-multishot-accept sqe fd addr addrlen flags)
  (%io-uring-set-target-fixed-file sqe (1- ioring-file-index-alloc)))

(defun io-uring-prep-cancel64 (sqe user-data flags)
  (io-uring-prep-rw +io-async-cancel+ sqe -1 nil 0 0)
  (setf (slot sqe 'addr) user-data)
  (setf (slot sqe 'cancel-flags) flags))

#+todo
(defun io-uring-prep-cancel (sqe user-data flags)
  (io-uring-prep-cancel64 sqe user-data flags))

(defun io-uring-prep-cancel-fd (sqe fd flags)
  (io-uring-prep-rw +io-async-cancel+ sqe fd nil 0 0)
  (setf (slot sqe 'cancel-flags) (logior flags ioring-async-cancel-fd)))

(defun io-uring-prep-link-timeout (sqe ts flags)
  (io-uring-prep-rw +io-link-timeout+ sqe -1 ts 1 0)
  (setf (slot sqe 'timeout-flags) flags))

(defun io-uring-prep-connect (sqe fd addr addrlen)
  (io-uring-prep-rw +io-connect+ sqe fd addr 0 addrlen))

(defun io-uring-prep-bind (sqe fd addr addrlen)
  (io-uring-prep-rw +io-bind+ sqe fd addr 0 addrlen))

(defun io-uring-prep-listen (sqe fd backlog)
  (io-uring-prep-rw +io-listen+ sqe fd 0 backlog 0))

(defun io-uring-prep-epoll-wait (sqe fd events maxevents flags)
  (io-uring-prep-rw +io-epoll-wait+ sqe fd events maxevents 0)
  (setf (slot sqe 'rw-flags) flags))

(defun io-uring-prep-files-update (sqe fds nr-fds offset)
  (io-uring-prep-rw +io-files-update+ sqe -1 fds nr-fds offset))

(defun io-uring-prep-fallocate (sqe fd mode offset len)
  (io-uring-prep-rw +io-fallocate+ sqe fd 0 mode offset)
  (setf (slot sqe 'addr) len))

(defun io-uring-prep-openat (sqe dfd path flags mode)
  (io-uring-prep-rw +io-openat+ sqe dfd path mode 0)
  (setf (slot sqe 'open-flags) flags))

(defun io-uring-prep-openat-direct (sqe dfd path flags mode file-index)
  (io-uring-prep-openat sqe dfd path flags mode)
  (when (= file-index ioring-file-index-alloc) (decf file-index))
  (%io-uring-set-target-fixed-file sqe file-index))

(defun io-uring-prep-open (sqe path flags mode)
  (io-uring-prep-openat sqe sys:at-fdcwd path flags mode))

(defun io-uring-prep-open-direct (sqe path flags mode file-index)
  (io-uring-prep-openat-direct sqe sys:at-fdcwd path flags mode file-index))

(defun io-uring-prep-close (sqe fd)
  (io-uring-prep-rw +io-close+ sqe fd nil 0 0))

(defun io-uring-prep-close-direct (sqe file-index)
  (io-uring-prep-close sqe 0)
  (%io-uring-set-target-fixed-file sqe file-index))

(defun io-uring-prep-read (sqe fd buf nbytes offset)
  (io-uring-prep-rw +io-read+ sqe fd buf nbytes offset))

(defun io-uring-prep-read-multishot (sqe fd nbytes offset buf-group)
  (io-uring-prep-rw +io-read-multishot+ sqe fd nil nbytes offset)
  (setf (slot sqe 'buf-group) buf-group
        (slot sqe 'flags) iosqe-buffer-select))

(defun io-uring-prep-write (sqe fd buf nbytes offset)
  (io-uring-prep-rw +io-write+ sqe fd buf nbytes offset))

(defun io-uring-prep-statx (sqe dfd path flags mask statxbuf)
  (io-uring-prep-rw +io-statx+ sqe dfd path mask statxbuf)
  (setf (slot sqe 'statx-flags) flags))

(defun io-uring-prep-fadvise (sqe fd offset len advice)
  (io-uring-prep-rw +io-fadvise+ sqe fd nil len offset)
  (setf (slot sqe 'fadvise-advice) advice))

(defun io-uring-prep-madvise (sqe addr len advice)
  (io-uring-prep-rw +io-madvise+ sqe -1 addr len 0)
  (setf (slot sqe 'fadvise-advice) advice))

(defun io-uring-prep-fadvise64 (sqe fd offset len advice)
  (io-uring-prep-rw +io-fadvise+ sqe fd nil 0 offset)
  (setf  (slot sqe 'addr) len
         (slot sqe 'fadvise-advice) advice))

(defun io-uring-prep-madvise64 (sqe addr len advice)
  (io-uring-prep-rw +io-madvise+ sqe -1 addr 0 len)
  (setf (slot sqe 'fadvise-advice) advice))

(defun io-uring-prep-send (sqe sockfd buf len flags)
  (io-uring-prep-rw +io-send+ sqe sockfd buf len 0)
  (setf (slot sqe 'msg-flags) flags))

(defun io-uring-prep-send-bundle (sqe sockfd len flags)
  (io-uring-prep-send sqe sockfd nil len flags)
  (setf (slot sqe 'ioprio) (logior (slot sqe 'ioprio) ioring-recvsend-bundle)))

(defun io-uring-prep-send-set-addr (sqe dest-addr addr-len)
  (setf (slot sqe 'addr2) dest-addr
        (slot sqe 'addr-len) addr-len))

(defun io-uring-prep-sendto (sqe sockfd buf len flags addr addrlen)
  (io-uring-prep-send sqe sockfd buf len flags)
  (io-uring-prep-send-set-addr sqe addr addrlen))

(defun io-uring-prep-send-zc (sqe sockfd buf len flags zc-flags)
  (io-uring-prep-rw +io-send-zc+ sqe sockfd buf len 0)
  (setf (slot sqe 'msg-flags) flags
        (slot sqe 'ioprio) zc-flags))

(defun io-uring-prep-send-zc-fixed (sqe sockfd buf len flags zc-flags buf-index)
  (io-uring-prep-send-zc sqe sockfd buf len flags zc-flags)
  (setf (slot sqe 'ioprio) (logior (slot sqe 'ioprio) ioring-recvsend-fixed-buf)
        (slot sqe 'buf-index) buf-index))

(defun io-uring-prep-sendmsg-zc (sqe fd msg flags)
  (io-uring-prep-sendmsg sqe fd msg flags)
  (setf (slot sqe 'opcode) +io-sendmsg-zc+))

(defun io-uring-prep-sendmsg-zc-fixed (sqe fd msg flags buf-index)
  (io-uring-prep-sendmsg-zc sqe fd msg flags)
  (setf (slot sqe 'ioprio) (logior (slot sqe 'ioprio) ioring-recvsend-fixed-buf)
        (slot sqe 'buf-index) buf-index))

(defun io-uring-prep-recv (sqe sockfd buf len flags)
  (io-uring-prep-rw +io-recv+ sqe sockfd buf len 0)
  (setf (slot sqe 'msg-flags) flags))

(defun io-uring-prep-recv-multishot (sqe sockfd buf len flags)
  (io-uring-prep-recv sqe sockfd buf len flags)
  (setf (slot sqe 'ioprio) (logior (slot sqe 'ioprio) ioring-recv-multishot)))

;; ... recvmsg payload
(defun io-uring-recvmsg-validate (buf buf-len msgh)
  (let ((hdr (alien-size io-uring-recvmsg-out))
        (namelen (slot msgh 'msg-namelen))
        (controllen (slot msgh 'msg-controllen)))
    (unless (or (< buf-len 0) (< buf-len hdr)
                (> namelen (- buf-len hdr))
                (> controllen (- buf-len hdr namelen)))
      (sap-alien buf (* io-uring-recvmsg-out)))))

(defun io-uring-recvmsg-name (o)
  (deref o 1))

(defun io-uring-recvmsg-cmsg-firsthdr (o msgh)
  (unless (< (slot o 'controllen) (alien-size cmsghdr))
    (sap-alien (int-sap (+ (sap-int (io-uring-recvmsg-name o)) (sap-int (slot msgh 'msg-namelen)))) (* cmsghdr))))

(defun io-uring-recvmsg-cmsg-nexthdr (o msgh cmsg)
  (unless (< (slot cmsg 'len) (alien-size cmsghdr))
    (let ((end (+ (sap-int (alien-sap (io-uring-recvmsg-cmsg-firsthdr o msgh))) (slot o 'controllen))) ;; (* unsigned-char)
          (cmsg (sap-alien (int-sap (+ (sap-int (alien-sap cmsg)) (slot cmsg 'cmsg-len))) (* cmsghdr))))
      (unless (or (> (1+ (sap-int (alien-sap cmsg)))  end) (> (+ (sap-int (alien-sap cmsg)) (slot cmsg 'sys::len)) end))
        cmsg))))

(defun io-uring-recvmsg-payload (o msgh)
  (int-sap (+ (sap-int (alien-sap (io-uring-recvmsg-name o))) (slot msgh 'namelen) (slot msgh 'controllen))))

(defun io-uring-recvmsg-payload-length (o buf-len msgh)
  (if (< buf-len 0)
      0
      (let ((start (sap-int (io-uring-recvmsg-payload o msgh)))
            (end (+ o buf-len)))
        (if (>= start end) 0 (- end start)))))

(defun io-uring-prep-openat2 (sqe dfd path how)
  ;; REVIEW 2026-04-10: 
  (io-uring-prep-rw +io-openat2+ sqe dfd path (alien-size open-how) (alien-sap how)))

(defun io-uring-prep-openat2-direct (sqe dfd path how file-index)
  (io-uring-prep-openat2 sqe dfd path how)
  (when (= file-index ioring-file-index-alloc) (decf file-index))
  (%io-uring-set-target-fixed-file sqe file-index))

(defun io-uring-prep-epoll-ctl (sqe epfd fd op ev)
  (io-uring-prep-rw +io-epoll-ctl+ sqe epfd ev op fd))

(defun io-uring-prep-provide-buffers (sqe addr len nr bgid bid)
  (io-uring-prep-rw +io-provide-buffers+ sqe nr addr len bid)
  (setf (slot sqe 'buf-group) bgid))

(defun io-uring-prep-remove-buffers (sqe nr bgid)
  (io-uring-prep-rw +io-remove-buffers+ sqe nr nil 0 0)
  (setf (slot sqe 'buf-group) bgid))

(defun io-uring-prep-shutdown (sqe fd how)
  (io-uring-prep-rw +io-shutdown+ sqe fd nil how 0))

(defun io-uring-prep-unlinkat (sqe dfd path flags)
  (io-uring-prep-rw +io-unlinkat+ sqe dfd path 0 0)
  (setf (slot sqe 'unlink-flags) flags))

(defun io-uring-prep-unlink (sqe path flags)
  (io-uring-prep-unlinkat sqe at-fdcwd path flags))

(defun io-uring-prep-renameat (sqe olddfd oldpath newdfd newpath flags)
  (io-uring-prep-rw +io-renameat+ sqe olddfd oldpath newdfd newpath)
  (setf (slot sqe 'rename-flags) flags))

(defun io-uring-prep-rename (sqe oldpath newpath)
  (io-uring-prep-renameat sqe at-fdcwd oldpath at-fdcwd newpath 0))

(defun io-uring-prep-sync-file-range (sqe fd len offset flags)
  (io-uring-prep-rw +io-sync-file-range+ sqe fd nil len offset)
  (setf (slot sqe 'sync-range-flags) flags))

(defun io-uring-prep-mkdirat (sqe dfd path mode)
  (io-uring-prep-rw +io-mkdirat+ sqe dfd path mode 0))

(defun io-uring-prep-mkdir (sqe path mode)
  (io-uring-prep-mkdirat sqe at-fdcwd path mode))

(defun io-uring-prep-symlinkat (sqe target newdirfd linkpath)
  (io-uring-prep-rw +io-symlinkat+ sqe newdirfd target 0 linkpath))

(defun io-uring-prep-linkat (sqe olddfd oldpath newdfd newpath flags)
  (io-uring-prep-rw +io-linkat+ sqe olddfd oldpath newdfd newpath)
  (setf (slot sqe 'hardlink-flags) flags))

(defun io-uring-prep-link (sqe oldpath newpath flags)
  (io-uring-prep-linkat sqe at-fdcwd oldpath at-fdcwd newpath flags))

(defun io-uring-prep-msg-ring-cqe-flags (sqe fd len data flags cqe-flags)
  (io-uring-prep-rw +io-msg-ring+ sqe fd nil len data)
  (setf (slot sqe 'msg-ring-flags) (logior ioring-msg-ring-flags-pass flags)
        (slot sqe 'file-index) cqe-flags))

(defun io-uring-prep-msg-ring (sqe fd len data flags)
  (io-uring-prep-rw +io-msg-ring+ sqe fd nil len data)
  (setf (slot sqe 'msg-ring-flags) flags))

(defun io-uring-prep-msg-ring-fd (sqe fd source-fd target-fd data flags)
  ;; addr = IORING_MSG_SEND_FD
  (io-uring-prep-rw +io-msg-ring+ sqe fd 1 0 data)
  (setf (slot sqe 'addr3) source-fd)
  (when (= target-fd ioring-file-index-alloc) (decf target-fd))
  (%io-uring-set-target-fixed-file sqe target-fd)
  (setf (slot sqe 'msg-ring-flags) flags))

(defun io-uring-prep-msg-ring-fd-alloc (sqe fd source-fd data flags)
  ;; addr = IORING_MSG_SEND_FD
  (io-uring-prep-msg-ring-fd sqe fd source-fd ioring-file-index-alloc data flags))

(defun io-uring-prep-getxattr (sqe name value path len)
  (io-uring-prep-rw +io-getxattr+ sqe 0 name len value)
  (setf (slot sqe 'addr3) path
        (slot sqe 'xattr-flags) 0))

(defun io-uring-prep-fgetxattr (sqe fd name value len)
  (io-uring-prep-rw +io-fgetxattr+ sqe fd name len value)
  (setf (slot sqe 'xattr-flags) 0))

(defun io-uring-prep-setxattr (sqe name value path flags len)
  (io-uring-prep-rw +io-setxattr+ sqe 0 name len value)
  (setf (slot sqe 'addr3) path
        (slot sqe 'xattr-flags) flags))

(defun io-uring-prep-fsetxattr (sqe fd name value flags len)
  (io-uring-prep-rw +io-fsetxattr+ sqe fd name len value)
  (setf (slot sqe 'xattr-flags) flags))

(defun io-uring-prep-socket (sqe domain type protocol flags)
  (io-uring-prep-rw +io-socket+ sqe domain nil protocol type)
  (setf (slot sqe 'rw-flags) flags))

(defun io-uring-prep-socket-direct (sqe domain type protocol file-index flags)
  (io-uring-prep-rw +io-socket+ sqe domain nil protocol type)
  (setf (slot sqe 'rw-flags) flags)
  (when (= file-index ioring-file-index-alloc) (decf file-index))
  (%io-uring-set-target-fixed-file sqe file-index))

(defun io-uring-prep-socket-direct-alloc (sqe domain type protocol flags)
  (io-uring-prep-rw +io-socket+ sqe domain nil protocol type)
  (setf (slot sqe 'rw-flags) flags)
  (%io-uring-set-target-fixed-file sqe (1- ioring-file-index-alloc)))

(defun %io-uring-prep-uring-cmd (sqe op cmd-op fd)
  (setf (slot sqe 'opcode) op
        (slot sqe 'fd) fd
        (slot sqe 'cmd-op) cmd-op
        (slot sqe 'pad) 0
        (slot sqe 'addr) 0
        (slot sqe 'len) 0))

(defun io-uring-prep-uring-cmd (sqe cmd-op fd)
  (%io-uring-prep-uring-cmd sqe +io-uring-cmd+ cmd-op fd))

(defun io-uring-prep-uring-cmd128 (sqe cmd-op fd)
  (%io-uring-prep-uring-cmd sqe +io-uring-cmd128+ cmd-op fd))

(defun io-uring-prep-cmd-sock (sqe cmd-op fd level optname optval optlen)
  (io-uring-prep-uring-cmd sqe cmd-op fd)
  (setf (slot sqe 'optval) optval
        (slot sqe 'optname) optname
        (slot sqe 'optlen) optlen
        (slot sqe 'level) level))

(defun io-uring-prep-cmd-getsockname (sqe fd sockaddr sockaddr-len peer)
  (io-uring-prep-uring-cmd sqe #.(std/alien::alien-enum-value 'io-uring-socket-op 'socket-uring-op-getsockname) fd)
  (setf (slot sqe 'addr) sockaddr
        (slot sqe 'addr3) sockaddr-len
        (slot sqe 'optlen) peer))

(defun io-uring-prep-waitid (sqe idtype id infop options flags)
  (io-uring-prep-rw +io-waitid+ sqe id nil idtype 0)
  (setf (slot sqe 'waitid-flags) flags
        (slot sqe 'file-index) options
        (slot sqe 'addr2) infop))

(defun io-uring-prep-futex-wake (sqe futex val mask futex-flags flags)
  (io-uring-prep-rw +io-futex-wake+ sqe futex-flags futex 0 val)
  (setf (slot sqe 'futex-flags) flags
        (slot sqe 'addr3) mask))

(defun io-uring-prep-futex-wait (sqe futex val mask futex-flags flags)
  (io-uring-prep-rw +io-futex-wait+ sqe futex-flags futex 0 val)
  (setf (slot sqe 'futex-flags) flags
        (slot sqe 'addr3) mask))

(defun io-uring-prep-futex-waitv (sqe futex nr-futex flags)
  (io-uring-prep-rw +io-futex-waitv+ sqe 0 futex nr-futex 0)
  (setf (slot sqe 'futex-flags) flags))

(defun io-uring-prep-fixed-fd-install (sqe fd flags)
  (io-uring-prep-rw +io-fixed-fd-install+ sqe fd nil 0 0)
  (setf (slot sqe 'flags) iosqe-fixed-file
        (slot sqe 'install-fd-flags) flags))

(defun io-uring-prep-ftruncate (sqe fd len)
  (io-uring-prep-rw +io-ftruncate+ sqe fd 0 0 len))

(defun io-uring-prep-cmd-discard (sqe fd offset nbytes)
  (io-uring-prep-uring-cmd sqe sys:block-uring-cmd-discard fd)
  (setf (slot sqe 'addr) offset
        (slot sqe 'addr3) nbytes))

(defun io-uring-prep-pipe (sqe fds pipe-flags)
  (io-uring-prep-rw +io-pipe+ sqe 0 fds 0 0)
  (setf (slot sqe 'pipe-flags) pipe-flags))

(defun io-uring-prep-pipe-direct (sqe fds pipe-flags file-index)
  (io-uring-prep-pipe sqe fds pipe-flags)
  (when (= file-index ioring-file-index-alloc) (decf file-index))
  (%io-uring-set-target-fixed-file sqe file-index))

;; (with-io-uring (ring)
;;   (io-uring-queue-init 160 ring 1)
;;   (io-uring-get-sqe ring))

(definline io-uring-load-sq-head (ring)
  (if (< 0 (logand (slot ring 'flags) ioring-setup-sqpoll))
      (barrier (:write) (deref (slot (slot ring 'sq) 'khead)))
      (deref (slot (slot ring 'sq) 'khead))))

(definline io-uring-sq-ready (ring)
  (declare ((alien (* io-uring)) ring))
  (- (slot (slot ring 'sq) 'sqe-tail) (io-uring-load-sq-head ring)))

(definline io-uring-sq-space-left (ring)
  (declare ((alien (* io-uring)) ring))
  (- (slot (slot ring 'sq) 'ring-entries) (io-uring-sq-ready ring)))

(definline io-uring-sqe-shift-from-flags (flags)
  (lognot (lognot (logand flags ioring-setup-sqe128))))

(definline io-uring-sqe-shift (ring)
  (declare ((alien (* io-uring)) ring))
  (io-uring-sqe-shift-from-flags (slot ring 'flags)))

(definline io-uring-sqring-wait (ring)
  (declare ((alien (* io-uring)) ring))
  (unless (or (zerop (logand (slot ring 'flags) ioring-setup-sqpoll))
              (not (zerop (io-uring-sq-space-left ring))))
    (%io-uring-sqring-wait ring)))

(definline io-uring-cq-ready (ring)
  (declare ((alien (* io-uring)) ring))
  (- (deref (slot (slot ring 'cq) 'ktail)) (deref (slot (slot ring 'cq) 'khead))))

(definline io-uring-cq-has-overflow (ring)
  ;; IO_URING_READ_ONCE()
  (logand (deref (slot (slot ring 'sq) 'kflags)) ioring-sq-cq-overflow))

(definline io-uring-cq-eventfd-enabled (ring)
  (declare ((alien (* io-uring)) ring))
  (or (zerop (deref (slot (slot ring 'cq) 'kflags)))
      (zerop (logand (deref (slot (slot ring 'cq) 'kflags)) ioring-cq-eventfd-disabled))))

(definline io-uring-cq-eventfd-toggle (ring enabled)
  (declare ((alien (* io-uring)) ring))
  (unless (and (io-uring-cq-eventfd-enabled ring) enabled)
    (if (zerop (deref (slot (slot ring 'cq) 'kflags)))
        (- sb-posix:eopnotsupp)
        (let ((flags (deref (slot (slot ring 'cq) 'kflags))))
          (if enabled
              (setf flags (lognand flags ioring-cq-eventfd-disabled))
              (setf flags (logior flags ioring-cq-eventfd-disabled)))
          ;; IO_URING_WRITE_ONCE()
          (setf (deref (slot (slot ring 'cq) 'kflags)) flags)))))

(definline io-uring-wait-cqe-nr (ring cqe-ptr wait-nr)
  (declare ((alien (* io-uring)) ring))
  (%io-uring-get-cqe ring cqe-ptr 0 wait-nr nil))

(definline io-uring-skip-cqe (ring cqe err)
  (declare ((alien (* io-uring)) ring))
  (block .check
    (cond 
      ((not (zerop (logand (slot cqe 'flags) ioring-cqe-f-skip))) (return-from .check nil))
      ((or (not (zerop (logand (slot ring 'features) ioring-feat-ext-arg)))
           (not (= (slot cqe 'user-data) -1)))
       (return-from io-uring-skip-cqe nil))
      ((< (slot cqe 'res) 0) (setf (deref err) (slot cqe 'res)))))
  (io-uring-cq-advance ring (io-uring-cqe-nr cqe))
  (lognot (deref err)))

(definline %io-uring-peek-cqe (ring cqe-ptr nr-available)
  (with-alien ((cqe (* io-uring-cqe))
               (err int 0)
               (available unsigned)
               (mask unsigned (slot (slot ring 'cq) 'ring-mask))
               (shift unsigned (io-uring-cqe-shift ring)))
    (loop
      (let ((tail (sap-int (alien-sap (slot (slot ring 'cq) 'ktail))))
            (head (sap-int (alien-sap (slot (slot ring 'cq) 'khead)))))
        (setf cqe nil
              available (- tail head))
        (when (zerop available) (return nil))
        (setf cqe (addr (deref (slot (slot ring 'cq) 'cqes) (ash (logand head mask) shift))))
        (when (not (io-uring-skip-cqe ring cqe (addr err))) (return nil))
        (setf cqe nil)))
    (setf (deref cqe-ptr) cqe)
    (when nr-available (setf (deref nr-available) available))
    err))

(definline io-uring-peek-cqe (ring cqe-ptr)
  (if (and (not (plusp (%io-uring-peek-cqe ring cqe-ptr nil))) (deref cqe-ptr))
      0
      (io-uring-wait-cqe-nr ring cqe-ptr 0)))

(definline io-uring-wait-cqe (ring cqe-ptr)
  (declare ((alien (* io-uring)) ring) ((alien (* (* io-uring-cqe))) cqe-ptr))
  (if (and (not (plusp (%io-uring-peek-cqe ring cqe-ptr nil))) (not (null-alien (deref (deref cqe-ptr)))))
      0
      (io-uring-wait-cqe-nr ring cqe-ptr 1)))

(definline io-uring-buf-ring-mask (ring-entries)
  (1- ring-entries))

(definline io-uring-buf-ring-init (br)
  (setf (slot br 'tail) 0))

(defun io-uring-buf-ring-add (br addr len bid mask buf-offset)
  (with-alien ((buf (* io-uring-buf) (addr (deref (slot br 'bufs) (logand (+ (slot br 'tail) buf-offset) mask)))))
    (setf (slot buf 'addr) addr
          (slot buf 'len) len
          (slot buf 'bid) bid)))

(defun io-uring-buf-ring-advance (br count)
  (with-alien ((new-tail unsigned-short (+ (slot br 'tail) count)))
    ;; io-uring-smp-store-release
    (setf (slot br 'tail) new-tail)))

(defun %io-uring-buf-ring-cq-advance (ring br cq-count buf-count)
  (io-uring-buf-ring-advance br buf-count)
  (io-uring-cq-advance ring cq-count))

(defun io-uring-buf-ring-cq-advance (ring br count)
  (%io-uring-buf-ring-cq-advance ring br count count))

(defun io-uring-buf-ring-available (ring br bgid)
  (with-alien ((head unsigned-short)
               (ret int))
    (setf ret (io-uring-buf-ring-head ring bgid (addr head)))
    (if (plusp ret) ret (- (slot br 'tail) head))))

#|
* Return an sqe to fill. Application must later call io_uring_submit()
* when it's ready to tell the kernel about it. The caller may call this
* function multiple times before calling io_uring_submit().
*
* Returns a vacant sqe, or NULL if we're full.
|#
(defun io-uring-get-sqe (ring)
  (declare ((alien (* io-uring)) ring))
  (let* ((sq (addr (slot ring 'sq)))
         (head (io-uring-load-sq-head ring))
         (tail (slot sq 'sqe-tail))
         (sqe (make-alien io-uring-sqe)))
    (unless (>= (- tail head) (slot sq 'ring-entries))
      (setf sqe (addr (deref (slot sq 'sqes) (ash (logand tail (slot sq 'ring-mask)) (io-uring-sqe-shift ring))))
            (slot sq 'sqe-tail) (1+ tail))
      (io-uring-initialize-sqe sqe)
      sqe)))

#|
* Return a 128B sqe to fill. Applications must later call io_uring_submit()
* when it's ready to tell the kernel about it. The caller may call this
* function multiple times before calling io_uring_submit().
*
* Returns a vacant 128B sqe, or NULL if we're full. If the current tail is the
* last entry in the ring, this function will insert a nop + skip complete such
* that the 128b entry wraps back to the beginning of the queue for a
* contiguous big sq entry. It's up to the caller to use a 128b opcode in order
* for the kernel to know how to advance its sq head pointer.
|#
;; FIX 2026-04-13: 
(defun io-uring-get-sqe128 (ring)
  (declare ((alien (* io-uring)) ring))
  (let* ((sq (addr (slot ring 'sq)))
         (head (io-uring-load-sq-head ring))
         (tail (slot sq 'sqe-tail))
         (sqe (make-alien io-uring-sqe)))
    (cond 
      ((= 1 (logand (slot ring 'flags) ioring-setup-sqe128)) (return-from io-uring-get-sqe128 (io-uring-get-sqe ring)))
      ((/= 1 (logand (slot ring 'flags) ioring-setup-sqe-mixed)) (return-from io-uring-get-sqe128 nil))
      ((zerop (logand (1+ tail) (slot sq 'ring-mask)))
       (when (>= (- (+ tail 2) head) (slot sq 'ring-entries)) (return-from io-uring-get-sqe128 nil))
       (setf sqe (io-uring-get-sqe ring))
       (io-uring-prep-nop sqe)
       (setf (slot sqe 'flags) (logior (slot sqe 'flags) iosqe-cqe-skip-success)
             tail (slot sq 'sqe-tail)))
      ((>= (- (1+ tail) head) (slot sq 'ring-entries)) (return-from io-uring-get-sqe128 nil)))
    (setf sqe (addr (deref (slot sq 'sqes) (logand tail (slot sq 'ring-mask))))
          (slot sq 'sqe-tail) (+ tail 2))
    (io-uring-initialize-sqe sqe)
    sqe))
