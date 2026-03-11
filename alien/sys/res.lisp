;;; res.lisp --- Resource bindings

;; 

;;; Code:
(in-package :sys)

;;;; resources
(define-alien-type rlim-t unsigned-long)

(define-alien-type rlimit
    (struct rlimit
      (rlim-cur rlim-t)
      (rlim-max rlim-t)))

(defar getrlimit int
  (resource int)
  (rlimits (* rlimit)))
(defar setrlimit int
  (resource int)
  (rlimit (* rlimit)))
(defar getpriority int
  (which int)
  (who int))
(defar setpriority int
  (which int)
  (who int)
  (value int))
(defar nice int (inc int))
(defar sigaction int
  (signum int)
  (act (* t))
  (oldact (* t)))

(definline rlimit (res)
  (with-alien ((rl rlimit))
    (getrlimit res (addr rl))
    (values (slot rl 'rlim-cur) (slot rl 'rlim-max))))
