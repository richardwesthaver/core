;;; std/serde.lisp --- Basic Lisp Serializer API

;; Read/Write Lisp Objects.

;;; Commentary:

;; This package contains macros for defining a pair of functions for a
;; category of lisp types - a READ-* function and a WRITE-* function. These
;; functions operate on a storage context which we Serialize (write) and Deserialize
;; (read) values from.

;; Within the STD system we implement the API for octet vectors as well as
;; (ALIEN (* UNSIGNED-CHAR)). These are used by higher-level packages which
;; need to portably serialize lisp objects as octet vectors.

;;; Code:
(in-package :std/serde)

(eval-always
  (define-condition serde-condition () ())
  (deferror serde-error (serde-condition) ()))

(deferror serializer-error (serde-error) ())
(deferror deserializer-error (serde-error) ())

;;; Serialize
(defgeneric serializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil of object SELF is serializable."))

(defgeneric serialize (obj format &key)
  (:documentation "Serialize OBJ to FORMAT, which is a SERIALIZABLE-TYPE-DESIGNATOR."))
;;; Deserialize
(defgeneric deserializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil if object SELF is deserializable."))

(defgeneric deserialize (from format &key)
  (:documentation "Deserialize FROM into an object of type FORMAT, which is a
DESERIALIZABLE-TYPE-DESIGNATOR."))

(defgeneric ser (kind obj))
(defgeneric de (kind obj))

(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead
of relying on a type-designator format and generating an object in the
method body."))

(declaim (simple-vector *lisp-objects* *simple-lisp-objects*))
(defparameter *simple-lisp-objects* 
  (apply 'vector '(fixnum
                   character single-float 
                   double-float bignum
                   short-float complex
                   rational string
                   pathname symbol 
                   cons hash-table 
                   standard-object struct
                   array class 
                   null t)))
(defvar *lisp-objects* *simple-lisp-objects*)

(defun %lisp-object-id (obj)
  (position obj *lisp-objects*))

(defmacro define-io (name &body body)
  "Define a set of readers and writers of category NAME.

BODY contains elements of the form:

(OBJECT &KEY READ WRITE)"
  (when body
    `(progn
       (defmacro ,(symbolicate 'read- name) (ty from)
         `(,(intern (string (symbolicate 'read- ',name '- ty)) ,*package*) ,from))
       (defmacro ,(symbolicate 'write- name) (ty obj to)
         `(,(intern (string (symbolicate 'write- ',name '- ty)) ,*package*) ,to ,obj))
       ,@(loop for form in body
               append 
                  (let* ((type (if (consp #1=(car form))
                                   (format nil "~@[~{~A-~^~A~}~]" #1#)
                                   #1#))
                         (rfn (symbolicate 'read- name '- type))
                         (wfn (symbolicate 'write- name '- type)))
                    `(,@(when-let ((rf (cdr (assoc :read (cdr form)))))
                          (when #2=(cdr rf)
                                `((defun ,rfn ,(car rf) ,@(if (atom #2#) (list #2#) #2#)))))
                      ,@(when-let ((wf (cdr (assoc :write (cdr form)))))
                          (when #3=(cdr wf)
                                `((defun ,wfn ,(car wf) ,@(if (atom #3#) (list #3#) #3#)))))))))))

(defun serialize-bignum (frob buf)
  "Serialize bignum to buffer stream"
  (declare (integer frob)
           (static-stream buf))
  (let* ((num (abs frob))
         (word-size (ceiling (/ (integer-length num) 32)))
         (needed (* word-size 4))
         (byte-spec (byte 32 0)))
    (declare (type fixnum word-size needed)
             (type cons byte-spec)
             (ignorable byte-spec))
    (if (< frob 0) 
        (write-byte +negative-bignum+ buf)
        (write-byte +positive-bignum+ buf))
    (write-uint32 needed buf)
    (loop for i fixnum from 0 below word-size 
          do (write-uint32 (ldb (byte 32 (* 32 i)) num) buf))))

(defun deserialize-bignum (buf length positive)
  (declare (type static-stream buf)
           (type fixnum length)
           (type boolean positive))
  (let ((int-byte-spec (byte 32 0)))
    (declare (dynamic-extent int-byte-spec)
             (ignorable int-byte-spec))
    (loop for i from 0 below (/ length 4)
          for byte-spec = (byte 32 (the fixnum (* 32 i)))
          with num of-type integer = 0 
          do (setq num (dpb (read-uint32 buf) byte-spec num))
          finally (return (if positive num (- num))))))
