;;; uring/types.lisp --- concrete types

;; args,flags,etc

;;; Code:

(in-package :uring)

;; statx epoll-event __kernel_rwf_t

;; target fd/u32

;; sb-posix:file-descriptor - not yet allocated by uring
;; uring:fixed - file-descriptor that has been registered with uring

;;; Time

;; Default behavior is to treat the timespec C type as a relative time
;; interval.

;; flags may contain ABS to indicate absolute time. When using
;; absolute time, the kernel uses its monotonic clock unless flags
;; contain BOOTTIME/REALTIME
(load-uring)

;; ioring-msg -- enum

;; helper for parsing the result of a multishot
(defstruct recv-msg-out
  (header nil :type io-uring-recvmsg-out)
  (msghdr-name-len 0 :type fixnum)
  (name-data #() :type octet-vector)
  (control-data #() :type octet-vector)
  (payload-data #() :type octet-vector))


;; (defun parse-recv-msg-out (buf header) msghdr..)


;;; Cancel
(define-alien-type async-cancel-flags int)
(define-alien-type user-data unsigned-long)

(defstruct cancel-builder
  (flags 0 :type async-cancel-flags)
  (user-data nil :type user-data))

;; any, user_data, fd, all

(defstruct mmapped-region
  (addr nil :type system-area-pointer)
  (len 0 :type fixnum))

;; do-mmap
;; map len bytes starting from offset from file-descriptor in mmapped-region
