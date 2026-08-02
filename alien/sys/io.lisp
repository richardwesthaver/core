;;; io.lisp --- UIO

;; Linux uio.h

;;; Commentary:

;; This file provides support for IOVECs as used by Linux syscalls and
;; io_uring. IO-VECs/IO-VECTORs are based on FOREIGN-VECTORs but may refer to
;; discontinuous slices of memory. The IO-VECTOR-CLASS is similar to
;; FOREIGN-VECTOR-CLASS but in addition to dispatching on element-type we
;; dispatch on length too.

;; Note that IO-VEC refers to a single IOVEC instance, whereas an IO-VECTOR
;; refers to an array of IOVEC instances.

;; The ELEMENT-TYPE of IO-VECs and IO-VECTORs are effectively the same,
;; referring to the underlying element type itself (usually (unsigned-byte
;; 8)). The LENGTH for IO-VECs is self-evident, and for IO-VECTORs it is the
;; count of IOVECs (NOT the total count of elements, or the length of each
;; individual IOVEC).

;;; Code:
(in-package :sys)

(defclass io-vector-class (foreign-vector-class)
  ((length :initarg :length :initform 0 :type array-index :reader sequence:length))
  (:documentation "Foreign vectors which point to alien slice types (iovec for IO-VEC,
rocksdb_pinnableslice_t for RDB-SLICE)."))

(defclass io-vec ()
  ((sap :initarg :sap :initform nil :accessor sap))
  (:metaclass io-vector-class)
  (:documentation "Lisp representation of the iovec C type consisting of an address and a length."))

(defun io-vector-length (iv)
  (slot-value (class-of iv) 'length))

(defmethod sequence:length ((self io-vec)) (io-vector-length self))

;; memoize on element-type and length
(with-memoization ()
  (memoizing
   (defun io-vec (element-type length)
     (or (if-let ((class (find (cons element-type length)
                               (std/meta:class-direct-subclasses (find-class 'io-vec))
                               :key (lambda (k) (cons (element-type k) (sequence:length k)))
                               :test #'equalp)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<IO-VEC:~a.~a>" element-type length) (find-package "SYS"))))
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (io-vec) ()
                   (:metaclass io-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'element-type) ',element-type
                       (slot-value (find-class ',cl-name) 'length) ',length)))
             cl-name))))))

(defclass io-vector ()
  ((sap :initarg :sap :initform nil :accessor sap))
  (:metaclass io-vector-class)
  (:documentation "An alien array of IOVECs with the same length and element type."))

(with-memoization ()
  (memoizing
   ;; note that a length = 0 should mean infinite length/boundless
   ;; REVIEW 2026-04-13: maybe make this '* and align with simple-array
   (defun io-vector (element-type length)
     (or (if-let ((class (find (cons element-type length)
                               (std/meta:class-direct-subclasses (find-class 'io-vector))
                               :key (lambda (k) (cons (element-type k) (sequence:length k)))
                               :test #'equalp)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<IO-VECTOR:~a.~a>" element-type length) (find-package "SYS"))))
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (io-vector) ()
                   (:metaclass io-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'element-type) ',element-type
                       (slot-value (find-class ',cl-name) 'length) ',length)))
             cl-name))))))

(defun io-octet-vector (length)
  "Create a new IO-VECTOR of type OCTET with the provided LENGTH. The result is memoized."
  (make-instance (io-vector 'octet length) :sap (alien-sap (make-alien unsigned-char length))))

;; alloc free
;; (defun ioref (x i))
;; (defun (setf ioref) (val x i))
