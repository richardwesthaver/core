;;; mem.lisp --- CUDA Memory Allocation

;; 

;;; Code:
(in-package :cuda)

(defun alloc-device-memory (type n)
  (with-alien ((d cu-device-ptr))
    (cu-mem-alloc (addr d) (* n (std/alien::foreign-type-size type)))
    d))

(defmacro with-device-memory ((var type n) &body body)
  `(let ((,var (alloc-device-memory ,type ,n)))
     (unwind-protect (progn ,@body)
       (cu-mem-free ,var))))

(defun device-total-size (dev)
  (with-alien ((s size-t))
    (cu-device-total-mem (addr s) dev)
    s))

;;; MEMCPY
(defun memcpy-host-to-device (device-ptr host-ptr type n)
  (let ((size (foreign-type-size type)))
    (cu-memcpy-host-to-device device-ptr host-ptr (* n size))))

(defun memcpy-host-to-device-async (device-ptr host-ptr type n stream)
  (let ((size (foreign-type-size type)))
    (cu-memcpy-host-to-device-async device-ptr host-ptr (* n size) stream)))

(defun memcpy-device-to-host (host-ptr device-ptr type n)
  (let ((size (foreign-type-size type)))
    (cu-memcpy-device-to-host host-ptr device-ptr (* n size))))

(defun memcpy-device-to-host-async (host-ptr device-ptr type n stream)
  (let ((size (foreign-type-size type)))
    (cu-memcpy-device-to-host-async host-ptr device-ptr (* n size) stream)))

;;; Memblock
(defclass cuda-vector ()
  ((length :initarg :length :initform 0)
   (device-sap :initarg :device-sap :accessor device-sap))
  (:metaclass foreign-vector-class)
  (:documentation "Foreign vector class for CUDA memory shared between device and host. Instances
of this class are created with (MAKE-INSTANCE (CUDA-VECTOR 'ELT)) where ELT is
the element-type.

The CUDA-VECTOR function memoizes the resulting class instance which
subclasses both CUDA-VECTOR and FOREIGN-VECTOR. The HOST memory pointer is
accessed via the SAP slot and is compatible with FVREF."))

(with-memoization ()
  (memoizing
   (defun cuda-vector (element-type)
     (or (if-let ((class (find element-type (class-direct-subclasses (find-class 'cuda-vector)) :key #'element-type)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<CUDA-VECTOR: ~a>"  element-type) (find-package "CUDA"))))
             (assert (member #1=(element-type-to-alien element-type) '#.'(char c-string unsigned-char short unsigned-short int unsigned-int long unsigned-long float double)) nil 'invalid-argument :item #1# :reason "invalid element type")
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (foreign-vector cuda-vector) ()
                   (:metaclass foreign-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'element-type) ',element-type)))
             cl-name))))))

(defmethod sync ((self cuda-vector) &key (direction :host-to-device))
  (declare ((member :host-to-device :device-to-host) direction))
  (let ((device-ptr (sap self))
        (host-ptr (device-sap self))
        (type (foreign-vector-element-type self))
        (size (foreign-vector-length self)))
    (ecase direction
      (:host-to-device
       (memcpy-host-to-device device-ptr host-ptr type size))
      (:device-to-host
       (memcpy-device-to-host host-ptr device-ptr type size)))))

;; (defun curef (x i))
;; (defun (setf curef) (val x i))
