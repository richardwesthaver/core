#!/bin/core --script
#|IO Testing Utility
|#
(using :uring :sys :io)

(defparameter *block-size* 1024)
(defparameter *queue-depth* 1)

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

(defun submit-read-request (path ring)
  (let* ((size (file-size path))
         (remaining size)
         (offset 0)
         (current-block 0)
         (blocks (/ size *block-size*)))
    (when (mod size *block-size*) (incf blocks))
    ;; allocate file-info..
    ;; allocate input buffer..
    (loop while (plusp remaining)
          with bytes-to-read = remaining
          if (> bytes-to-read *block-size*)
          do (setf bytes-to-read *block-size*)
          do (incf offset bytes-to-read)
          ;; set length..
          ;; alloc base..
          do (incf current-block)
          do (decf remaining bytes-to-read))
    ;; set file-size of file-info..
    (with-alien ((sqe (* io-uring-sqe) (io-uring-get-sqe ring)))
      #+todo (uring::io-uring-prep-readv sqe file-fd iovecs blocks 0)
      #+todo (uring::io-uring-sqe-set-data sqe fi)
      (uring::io-uring-submit ring))))

(defun cat (&rest files)
  (with-new-io-uring r
    (uring::io-uring-queue-init *queue-depth* (addr r) 0)
    ;; loop across input args submit read for each
    (mapc (lambda (x) 
            (submit-read-request x (addr r))
            (get-completion-and-print (addr r)))
          files)
    (uring::io-uring-queue-exit (addr r))))

;; liburing cp
;; (defun cp (dst &rest src))
