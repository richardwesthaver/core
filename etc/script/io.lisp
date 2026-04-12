#!/bin/core --script
#|IO Testing Utility
|#
(in-package :uring)

(using :uring :sys :io)
;; liburing cat
;; io-uring-wait-cqe
(defun get-completion-and-print (ring)
  (let ((c (allocate-io-uring-cqe)))
    (let ((ret (io-uring-wait-cqe ring c)))
      (if (minusp ret) (error "~A" ret) (print c)))))

;; (with-io-uring (r) (get-completion-and-print r))

; file-size 
;; (defun cat (&rest files))

;; liburing cp
;; (defun cp (dst &rest src))
