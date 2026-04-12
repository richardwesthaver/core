;;; io.lisp --- UIO

;; Linux uio.h

;;; Commentary:

;; This file provides support for IOVECs as used by Linux syscalls and
;; io_uring. IO-VECTORs are based on FOREIGN-VECTORs but may refer to
;; discontinuous slices of memory.

;;; Code:
(in-package :sys)

(defclass io-vector-class (foreign-vector-class)
  ((length :initarg :length :initform 0 :type array-index)))

(defclass io-vec () 
  ((sap :initarg :sap :initform nil :accessor sap))
  (:metaclass io-vector-class))

(defun io-vector-length (iv)
  (slot-value (class-of iv) 'length))

(defmethod sequence:length ((self io-vec)) (io-vector-length self))

;; instead of memoizing based on element-type we 
(with-memoization ()
  (memoizing
   (defun io-vec (element-type length)
     (or (if-let ((class (find element-type (class-direct-subclasses (find-class 'io-vec)) :key #'element-type)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<IO-VEC: ~a>"  element-type) (find-package "SYS"))))
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (io-vec) ()
                   (:metaclass io-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'element-type) ',element-type
                       (slot-value (find-class ',cl-name) 'length) ',length)))
             cl-name))))))

;; An alien array of IOVECs with the same length and element type
(defclass io-vector ()
  ((sap :initarg :sap :initform nil :accessor sap))
  (:metaclass io-vector-class))

(with-memoization ()
  (memoizing
   (defun io-vector (element-type length)
     (or (if-let ((class (find element-type (class-direct-subclasses (find-class 'io-vector)) :key #'element-type)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<IO-VECTOR: ~a>"  element-type) (find-package "SYS"))))
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (io-vector) ()
                   (:metaclass io-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'element-type) ',element-type
                       (slot-value (find-class ',cl-name) 'length) ',length)))
             cl-name))))))
