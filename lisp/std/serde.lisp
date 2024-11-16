;;; std/serde.lisp --- Basic Lisp Serializer API

;; Read/Write Lisp Objects.

;;; Commentary:

;; This package contains macros for defining a pair of functions for a
;; category of lisp types - a READ-* function and a WRITE-* function. These
;; functions operate on a storage context which we Serialize (write) and Deserialize
;; (read) values from.

;; Within the STD system we implement the API for octet vectors as well as
;; (ALIEN (* UNSIGNED-CHAR)). These are used by higher-level packages which
;; need to portably serialize lisp objects as bytes.

;;; Code:
(in-package :std/serde)

(declaim (simple-vector *lisp-objects*))


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

(defmacro define-serde (name &body body)
  "Define a set of serializer and deserializers.

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
                  (let* ((type (let ((%ty (car form)))
                                 (if (consp %ty)
                                     (format nil "~@[~{~A-~^~A~}~]" %ty)
                                     %ty)))
                         (rfn (symbolicate 'read- name '- type))
                         (wfn (symbolicate 'write- name '- type)))
                    `(,@(when-let ((rf (cdr (assoc :read (cdr form)))))
                          (when #1=(cdr rf)
                                `((defun ,rfn ,(car rf) ,@(if (atom #1#) (list #1#) #1#)))))
                      ,@(when-let ((wf (cdr (assoc :write (cdr form)))))
                          (when #2=(cdr wf)
                                `((defun ,wfn ,(car wf) ,@(if (atom #2#) (list #2#) #2#)))))))))))
