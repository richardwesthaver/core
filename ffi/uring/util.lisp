;;; uring/types.lisp --- concrete types

;; args,flags,etc

;;; Code:

(in-package :uring)

(define-alien-type nil
  (struct sigset-t
          (val (array unsigned-long #.+sigset-nwords+))))

(define-alien-type cpu-mask-t unsigned-long)

(define-alien-type nil
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

(defstruct mmapped-region
  (addr (deref (make-alien (* t))) :type (alien (* t))) ;; (sb-impl::dynamic-space-free-pointer) ;?
  (len 0 :type fixnum))

;; do-mmap
;; map len bytes starting from offset from file-descriptor in mmapped-region

;;; CPU Affinity
;; it appears this actually crashes SBCL, receiving sig6 from foreign thread
;; (define-alien-routine sched-setaffinity int (pid int) (cpusetsize size-t) (set (* (struct cpu-set-t))))
;; (define-alien-routine sched-getaffinity int (pid int) (cpusetsize size-t) (set (* (struct cpu-set-t))))
;; (sched-getaffinity 0 cpu-setsize (make-alien (struct cpu-set-t)))
;; (sched-setaffinity 0 cpu-setsize (make-alien (struct cpu-set-t)))
