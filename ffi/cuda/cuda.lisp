;;; cuda.lisp --- CUDA API

;; 

;;; Code:
(in-package :cuda)

(defvar *cuda-device*)
(defvar *cuda-context*)

(defun device-compute-capability (dev-id)
  (with-alien ((maj int)
               (min int))
    (cu-device-compute-capability (addr maj) (addr min) dev-id)
    (values maj min)))

(defun get-nvcc-arch (dev-id)
  (multiple-value-bind (maj min) (device-compute-capability dev-id)
    (format nil "-arch=sm_~D~D" maj min)))

(defun get-cuda-device (dev-id)
  (with-alien ((dev cu-device))
    (cu-device-get (addr dev) dev-id)
    dev))

(defun create-cuda-context (dev)
  (with-alien ((ctx cu-context))
    (cu-ctx-create (addr ctx) 0 dev)
    ctx))

(defmacro with-cuda (dev-id &body body)
  `(progn
     (cu-init 0)
     (let* ((*cuda-device* (get-cuda-device ,dev-id))
            (*cuda-context* (create-cuda-context *cuda-device*)))
       (unwind-protect (progn ,@body)
         (cu-ctx-destroy *cuda-context*)))))

(defvar *cuda-stream* nil)
