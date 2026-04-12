#!/bin/core --script
#|IO Testing Utility
|#
(in-package :uring)

(using :uring :sys :io)
;; liburing cat
;; io-uring-wait-cqe
#+todo
(defun get-completion-and-print (ring)
  (with-io-cqe c
    (let ((ret (io-uring-wait-cqe ring (addr c))))
      (if (minusp ret) (error "~A" ret) (print c)))))
      
; file-size 
;; (defun cat (&rest files))

;; liburing cp
;; (defun cp (dst &rest src))
