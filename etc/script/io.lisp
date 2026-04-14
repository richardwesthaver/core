#!/bin/core --script
#|IO Testing Utility
|#
(using :uring :sys :io)
;; liburing cat
;; io-uring-wait-cqe
(defun get-completion-and-print (ring)
  (let ((c (make-alien (* io-uring-cqe))))
    (let ((ret (uring::io-uring-wait-cqe ring c)))
      (when (minusp ret) (error "~A" ret))
      (when (minusp (slot (print (deref (deref c))) 'uring::res))
        (error "async readv failed: ~A" ret))
      (print :ok)
      (uring::io-uring-cqe-get-data c))))

;; (uring:with-io-uring (r) (get-completion-and-print r))

; file-size 
;; (defun cat (&rest files))

;; liburing cp
;; (defun cp (dst &rest src))
