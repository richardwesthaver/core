;;; epoll.lisp --- EPOLL bindings

;; 

;;; Code:
(in-package :sys)

(define-alien-type epoll-data
  (union epoll-data-t
    (ptr (* t))
    (fd int)
    (u32 unsigned-int)
    (u64 unsigned-long)))

(define-alien-type epoll-event
  (struct epoll-event
    (events unsigned-int)
    (data epoll-data)))

(defar epoll-create int (size int))
;; only a single flag param accepted on current box - epoll-cloexec = #o2000000
(defar epoll-create1 int (flags int))
(defar epoll-ctl int
  (epfd int)
  (op int)
  (fd int)
  (event (* epoll-event)))
(defar epoll-wait int
  (epfd int)
  (events (* epoll-event))
  (maxevents int)
  (timeout int))
