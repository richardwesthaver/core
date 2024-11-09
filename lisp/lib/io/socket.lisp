;;; io/socket.lisp --- IO Sockets

;; 

;;; Code:
(in-package :io/socket)

(defun check-timeval (buffer size)
  (assert (= size #.(alien-size timeval :bytes)))
  buffer)

;;timevals
(defmacro define-socket-option-timeval (name level number &optional features (info ""))
  `(sb-bsd-sockets::define-socket-option ,name nil ,level ,number
     std/alien:timeval nil check-timeval addr ,features ,info))

(define-socket-option-timeval
    sockopt-receive-timeout sockint::sol-socket sockint::so-rcvtimeo)

(define-socket-option-timeval
    sockopt-send-timeout sockint::sol-socket sockint::so-sndtimeo)

;; linger
(define-alien-type linger 
  (struct linger
          (onoff int) ;; Nonzero to linger on close
          (linger int)))  ;; Time to linger

(defun check-linger (buffer size)
  (assert (= size #.(sb-alien:alien-size linger :bytes)))
  buffer)

(sb-bsd-sockets::define-socket-option sockopt-linger nil sockint::sol-socket sockint::so-linger
  linger nil check-linger sb-alien:addr)
  
  
;; ucre
(define-alien-type ucre
    (struct ucre
            (pid int)
            (uid int)
            (gid int)))

(defun check-ucre (buffer size)
  (assert (= size #.(sb-alien:alien-size ucre :bytes)))
  buffer)

(sb-bsd-sockets::define-socket-option sockopt-peercred nil sockint::sol-socket sockint::so-linger
  ucre nil check-ucre sb-alien:addr)
