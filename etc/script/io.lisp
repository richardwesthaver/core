#|IO Testing Utility - based on liburing examples (cat and cp).|#
(using :uring :sys :io :sb-alien)

(defparameter *block-size* 1024)
(defparameter *queue-depth* 1)

(defclass file-info ()
  ((size :initarg :size :type array-index)
   (iovecs :initarg :iovecs :type io-vector)))

(define-alien-type file-info
  (struct nil
    (size sys::off-t)
    (iovecs (* iovec))))

(defmethod pull-sap ((self file-info) (sap system-area-pointer))
  (unless (null-pointer-p sap)
    (with-alien ((finfo file-info (sap-alien sap file-info)))
      (let ((size (slot finfo 'size)))
        (make-instance 'file-info
          :size size
          :iovecs (make-instance (io-vector 'character (mod size *block-size*))
                    :sap (slot finfo 'iovecs)))))))

(defmethod push-sap* ((self file-info))
  (let ((fi (make-alien file-info)))
    (setf (slot fi 'size) (slot-value self 'size)
          (slot fi 'iovecs) (sap (slot-value self 'iovecs)))
    (alien-sap (deref fi))))

(defmethod push-sap ((self file-info) (sap system-area-pointer))
  (setf sap (push-sap* self)))

#|
(push-sap*
 (make-instance 'file-info 
   :size 420
   :iovecs (make-instance (io-vector 'octet 420)
             :sap (make-alien iovec 420))))
|#

;; liburing cat
;; io-uring-wait-cqe
(defun get-completion-and-print (ring)
  (let ((c (make-alien (* io-uring-cqe))))
    (let ((ret (uring::io-uring-wait-cqe ring c)))
      (when (minusp ret) (error "~A" ret))
      (when (minusp (slot (print (deref (deref c))) 'uring::res))
        (error "async readv failed: ~A" ret))
      (print :ok)
      (pull-sap (make-instance 'file-info) (alien-sap (uring::io-uring-cqe-get-data c))))))

(defun submit-read-request (path ring)
  (with-open-file (f path :direction :probe)
    (let* ((file-fd (stream-fd f))
           (size (file-length path))
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
    (with-alien ((sqe (* uring::io-uring-sqe) (uring::io-uring-get-sqe ring)))
      (uring::io-uring-prep-readv sqe file-fd iovecs blocks 0)
      (uring::io-uring-sqe-set-data sqe fi)
      (uring::io-uring-submit ring)))))

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
