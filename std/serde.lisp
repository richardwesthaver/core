;;; std/serde.lisp --- Basic Lisp Serializer API

;; Read/Write Lisp Objects. Binary object de/serialization.

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
  (define-condition serde-condition () ()
    (:documentation "Default SERDE condition class."))
  (deferror serde-error (serde-condition) ()
    (:documentation "An error signaled during serialization OR deserialization.")))

(deferror serializer-error (serde-error)
  ()
  (:documentation "An error which occurs during object serialization."))

(deferror deserializer-error (serde-error) 
  ()
  (:documentation "An error which occurs during object deserialization."))

(defgeneric serialize (obj format &key &allow-other-keys)
  (:documentation "Serialize OBJ to FORMAT."))

(defgeneric deserialize (from format &key &allow-other-keys)
  (:documentation "Deserialize FROM into an object of type FORMAT."))

(defgeneric ser (self)
  (:documentation "Access the serializer of SELF."))

(defgeneric (setf ser) (new self))

(defgeneric de (self)
  (:documentation "Access the deserializer of SELF."))

(defgeneric (setf de) (new self))

(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead of
relying on a designated format and generating an object in the method body."))

;; (defmacro defde (fmt &body body))
;; (defmacro defser (fmt &body body))
;; (defmacro defserde (fmt &body body))

(defglobal *io-table* (make-hash-table))

(defmacro define-io (name &body body)
  "Define a set of readers and writers of category NAME.

BODY contains elements of the form:

(OBJECT &KEY READ WRITE)"
  (when body
    `(progn
       (setf (gethash ,(std/sym:keywordicate name) *io-table*) '(:read nil :write nil))
       (defmacro ,(symbolicate 'read- name) (ty from)
         `(,(intern (string (symbolicate 'read- ',name '- ty)) ,*package*) ,from))
       (defmacro ,(symbolicate 'write- name) (ty obj to)
         `(,(intern (string (symbolicate 'write- ',name '- ty)) ,*package*) ,to ,obj))
       ,@(loop for form in body
               append 
                  (let* ((type (car form))
                         (type-name (if (consp type)
                                        (format nil "~@[~{~A-~^~A~}~]" type)
                                        type))
                         (rfn (symbolicate 'read- name '- type-name))
                         (wfn (symbolicate 'write- name '- type-name)))
                    `(,@(when-let ((rf (cdr (assoc :read (cdr form)))))
                          (when #1=(cdr rf)
                                `((defun ,rfn ,(car rf) ,@(if (atom #1#) (list #1#) #1#)))))
                      ,@(when-let ((wf (cdr (assoc :write (cdr form)))))
                          (when #2=(cdr wf)
                                `((defun ,wfn ,(car wf) ,@(if (atom #2#) (list #2#) #2#)))))))))))
