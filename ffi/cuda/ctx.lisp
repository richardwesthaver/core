;;; cuda.lisp --- CUDA API

;; 

;;; Code:
(in-package :cuda)

(defvar *cuda-device*)
(defvar *cuda-context*)
(defvar *cuda-stream* nil)

(defun device-compute-capability (dev-id)
  (with-alien ((maj int)
               (min int))
    (cu-device-compute-capability (addr maj) (addr min) dev-id)
    (values maj min)))

(defun get-cuda-device (dev-id)
  (with-alien ((dev cu-device))
    (cu-device-get (addr dev) dev-id)
    dev))

(defun create-cuda-context (dev)
  (with-alien ((ctx cu-context))
    (cu-ctx-create (addr ctx) 0 dev)
    ctx))

(defmacro with-cuda-context (dev-id &body body)
  `(let* ((*cuda-device* (get-cuda-device ,dev-id))
          (*cuda-context* (create-cuda-context *cuda-device*)))
     (unwind-protect (progn ,@body)
       (cu-ctx-destroy *cuda-context*))))

(defmethod init ((self (eql :cuda)) &key (id 0))
  (load-cuda)
  (cu-init 0)
  (setq *cuda-device* (get-cuda-device id)
        *cuda-context* (create-cuda-context *cuda-device*)))

