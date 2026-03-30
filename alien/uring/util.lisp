;;; uring/types.lisp --- concrete types

;; args,flags,etc

;;; Code:

(in-package :uring)

(define-alien-type sigset
  (struct sigset-t
    (val (array unsigned-long #.+sigset-nwords+))))

(define-alien-type cpu-mask-t unsigned-long)

(define-alien-type cpu-set
  (struct cpu-set-t
    (bits (array cpu-mask-t #.(/ +cpu-setsize+ +ncpu-bits+)))))

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

;; ioring-msg -- enum

;; helper for parsing the result of a multishot
(defstruct recv-msg-out
  (header (allocate-io-uring-recvmsg-out) :type (alien io-uring-recvmsg-out))
  (msghdr-name-len 0 :type fixnum)
  (name-data #() :type octet-vector)
  (control-data #() :type octet-vector)
  (payload-data #() :type octet-vector))

;; (defun parse-recv-msg-out (buf header) msghdr..)

;;; Cancel
(define-alien-type async-cancel-flags int)
(define-alien-type user-data unsigned-long)

(defstruct cancel-builder
  (flags 0 :type (alien async-cancel-flags))
  (user-data (deref (make-alien user-data)) :type (alien user-data)))

;; any, user_data, fd, all

;;; CPU Affinity
(define-alien-routine sched-setaffinity int (pid int) (cpusetsize size-t) (set (* (struct cpu-set-t))))
(define-alien-routine sched-getaffinity int (pid int) (cpusetsize size-t) (set (* (struct cpu-set-t))))
;; (sched-getaffinity 0 +cpu-setsize+ (make-alien (struct cpu-set-t)))
;; (sched-setaffinity 0 +cpu-setsize+ (make-alien (struct cpu-set-t)))
