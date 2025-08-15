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

;;; Serialize
(defgeneric serializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil of object SELF is serializable."))

(defgeneric serialize (obj format &key &allow-other-keys)
  (:documentation "Serialize OBJ to FORMAT, which is a SERIALIZABLE-TYPE-DESIGNATOR."))
;;; Deserialize
(defgeneric deserializable-p (self)
  (:method ((self t)) nil)
  (:documentation "Return non-nil if object SELF is deserializable."))

(defgeneric deserialize (from format &key &allow-other-keys)
  (:documentation "Deserialize FROM into an object of type FORMAT, which is a
DESERIALIZABLE-TYPE-DESIGNATOR."))

(defgeneric ser (self)
  (:documentation "Access the serializer of SELF."))
(defgeneric (setf ser) (new self))
(defgeneric de (self)
  (:documentation "Access the deserializer of SELF."))
(defgeneric (setf de) (new self))

(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead
of relying on a type-designator format and generating an object in the
method body."))

(defparameter *primitive-object-table*
  (let ((tbl (make-hash-table)))
    (dolist (obj *primitive-objects* tbl)
      (setf (gethash (primitive-object-name obj) tbl) (cons (symbol-value (primitive-object-lowtag obj)) (symbol-value (primitive-object-widetag obj))))))
  "Primitive objects are defined by SBCL and will not change. Convenient as a
non-unique ID prefix.")

(defvar *simple-type-table* (make-hash-table :test 'equal)
  "A hash-table mapping simple type names to integers.")

(defvar *simple-types* (make-array 128 :adjustable nil)
  "A vector containing the simple set of lisp objects .")

(defvar *core-type-table*)
(defvar *core-types*)

(defun reinitialize-core-types ()
  (setq *core-type-table* *simple-type-table*
        *core-types* *simple-types*))

(defun register-object-id (type id &optional (table *core-type-table*) (vector *core-types*))
  (setf (gethash type table) id
        (aref vector id) type))

(macrolet ((simple-id (type id)
             `(register-object-id ,type ,id *simple-type-table* *simple-types*))
           (simple-id-order (&rest types &aux (i 0))
             `(progn
                ,@(mapcar (lambda (x) (prog1 `(simple-id ',x ,i) (incf i))) types))))
  (simple-id-order 
   t
   character base-char 
   double-float  single-float 
   (complex double-float) (complex single-float) 
   integer
   bignum
   fixnum
   bit 
   symbol 
   boolean
   null cons 
   standard-object structure-object
   pathname hash-table
   array vector 
   string
   simple-array simple-vector 
   simple-string base-string
   octet-vector)
  (reinitialize-core-types))

;; TODO 2025-08-14: 
(defmacro simple-type-id (obj)
  `(typecase ,obj
     ,@(mapcar (lambda (x) (list (car x) (cdr x))) (std/hash-table:hash-table-alist *simple-type-table*))))

(defun get-core-type-id (obj)
  (or (gethash (type-of obj) *core-type-table*)
      (gethash (simple-type-id obj) *core-type-table*)))

(definline prim-type (obj)
  "Return the name of the primitive type of OBJ."
  (sb-vm::primitive-type-name (sb-vm::primitive-type-of obj)))

(definline core-type-id (obj)
  "Return the 'core-type-id' of OBJ which is a 16-bit integer containing type
information. The first 8 bits are the associated object widetag followed by an
8-bit tag corresponding to an index of the *CORE-OBJECTS* vector, which may be
extended by the user using the REGISTER-OBJECT-ID function. "
  (let ((id 0))
    (declare ((unsigned-byte 16) id) (dynamic-extent id))
    (setf (ldb (byte 8 0) id) (widetag-of obj)) ;; 8 bits
    (setf (ldb (byte 4 1) id) (get-core-type-id obj))
    id))

;; (defun %lisp-metaclass-id (obj))

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
