;;; gen.lisp --- Parquet Lisp Code Generator

;; 

;;; Code:
(defpackage :dat/parquet/gen ;; not public API
  (:use :cl :std :dat/proto :dat/json)
  (:export :load-parquet))

(in-package :dat/parquet/gen)
(defparameter *parquet-json-file*
  (probe-file #.(asdf:system-relative-pathname :prelude #P"../.stash/parquet.json")))
(defvar *parquet-json* nil)
(defun load-parquet-json (&optional (json-file *parquet-json-file*))
  (with-open-file (file json-file)
    (setq *parquet-json* (json-read file))))

(defun %parquet-json-enums ()
  (json-getf *parquet-json* "enums"))  

(defun parquet-json-enum-getf (name)
  (json-getf
   (find-if (lambda (x) (equal name (json-getf x "name"))) (%parquet-json-enums))
   "members"))

(defvar *parquet-enums* nil)

(defmacro def-parquet-enum (sym name)
  `(progn
     (defun ,(symbolicate "PARQUET-JSON-" sym) ()
       (mapcar (lambda (x) (keywordicate (snakecase-name-to-lisp-name (json-getf x "name"))))
               (parquet-json-enum-getf ,name)))
     (defparameter ,(intern
                     (concatenate 'string "*PARQUET-" (symbol-name sym) "*")
                     :dat/parquet)
       (,(symbolicate "PARQUET-JSON-" sym)))))

(defun camelcase-name-to-lisp-name (string)
  (string-upcase
   (with-output-to-string (name)
     (loop for i from 0 below (length string)
           for c across string
           when (and (upper-case-p c) (not (zerop i)))
           do (write-char #\- name)
           do (write-char c name)))))

(defun snakecase-name-to-lisp-name (string)
  (string-upcase
   (substitute #\- #\_ string)))

(labels ((parse-type-id (type-id)
           (string-case (type-id :default nil)
             ("bool" 'boolean)
             ("byte" 'signed-byte)
             ("i16" '(signed-byte 16))
             ("i32" '(signed-byte 32))
             ("i64" '(signed-byte 64))
             ("double" 'double-float)
             ("string" 'string)
             ("list" 'list)
             ("binary" 'octet-vector)
             ("set" 'list)))
         (parse-type (o)
           (let ((name (string-case ((json-getf o "typeId"))
                         ("union" (json-getf o "class"))
                         ("struct" (json-getf o "class"))
                         ("enum" (json-getf o "class")))))
             (intern
              (cond 
                ((equal name "UUIDType") "PARQUET-UUID-TYPE")
                (t (concatenate 'string
                         "PARQUET-"
                         (camelcase-name-to-lisp-name name))))
              :dat/parquet))))
  (defun convert-parquet-struct-field-type (field) ;; technically part of thrift type system
    (let* ((type-id (parquet-struct-field-type-id field))
           (type (parquet-struct-field-type field))
           (required (parquet-struct-field-required field))
           (unit-type (or (when type-id (parse-type-id type-id)) (when type (parse-type type)))))
      (if (and (equal "optional" required) (not (equal unit-type 'list))) ;; (listp nil) = t
          `(or null ,unit-type)
          unit-type))))

(defun parquet-json-enums ()
  (list
   (def-parquet-enum types "Type")
   (def-parquet-enum converted-types "ConvertedType")
   (def-parquet-enum field-repetition-types "FieldRepetitionType")
   (def-parquet-enum encodings "Encoding")
   (def-parquet-enum compression-codecs "CompressionCodec")
   (def-parquet-enum page-types "PageType")
   (def-parquet-enum boundary-orders "BoundaryOrder")))

(defvar *parquet-structs* nil)
(defstruct (parquet-struct
            (:constructor make-parquet-struct (name doc exceptionp unionp fields)))
  name doc exceptionp unionp (fields nil :type list))

(defstruct (parquet-struct-field
            (:constructor make-parquet-struct-field (key name type-id type doc required)))
  key name type-id type doc required)

(defun parquet-destruct-field (field)
  (list (parquet-struct-field-name field)
        (parquet-struct-field-key field)
        (parquet-struct-field-doc field)
        (parquet-struct-field-type-id field)
        (parquet-struct-field-type field)
        (parquet-struct-field-required field)))

(defun parquet-destruct (struct)
  (list (parquet-struct-name struct)
        (parquet-struct-doc struct)
        (parquet-struct-unionp struct)
        (parquet-struct-exceptionp struct)
        (mapcar #'parquet-destruct-field (parquet-struct-fields struct))))

(defun parquet-json-structs () ;; name doc isException isUnion fields
  (mapcar
   (lambda (s)
     (let ((name (json-getf s "name"))
           (doc (json-getf s "doc"))
           (exceptionp (json-getf s "isException"))
           (unionp (json-getf s "isUnion"))
           (fields (loop for f in (json-getf s "fields")
                         collect
                            (let ((key (json-getf f "key"))
                                  (name (json-getf f "name"))
                                  (type-id (json-getf f "typeId"))
                                  ;; json object - needs additional parsing
                                  (type (json-getf f "type"))
                                  (doc (json-getf f "doc"))
                                  (required (json-getf f "required")))
                              (make-parquet-struct-field key name type-id type doc required)))))
       (make-parquet-struct name doc exceptionp unionp fields)))
   (json-getf *parquet-json* "structs")))

(defun parquet-json-namespaces ()
  (json-getf *parquet-json* "namespaces"))

(defun init-parquet-json (&optional (file *parquet-json-file*))
  (load-parquet-json file)
  (setq *parquet-enums* (parquet-json-enums))
  (setq *parquet-structs* (parquet-json-structs)))

;;; CLOS
(defclass parquet-object () ())

;; (defmethod print-object ((obj parquet-object) stream)
;;   "Output a Parquet object to a stream."
;;   (print-unreadable-object (obj stream :type t)))

(defmacro define-parquet-class (name superclasses slots &rest options)
  "Define a new subclass of PARQUET-OBJECT with NAME."
  `(defclass ,name ,@(if-let ((s superclasses)) (list s) `((parquet-object))) ,slots ,@options))

;;; Codegen

;; 8)
(eval-always
  (defun %define-parquet-structs ()
    "Define all known values in *PARQUET-STRUCTS* using DEFINE-PARQUET-CLASS (DEFCLASS)."
    (loop for struct in *parquet-structs*
          unless (null struct)
          collect (let ((name (parquet-struct-name struct))
                        (doc (parquet-struct-doc struct))
                        (fields (parquet-struct-fields struct)))
                    `(define-parquet-class ,(intern (cond
                                                      ((equal name "UUIDType") "PARQUET-UUID-TYPE")
                                                      (t (concatenate 'string
                                                                      "PARQUET-"
                                                                      (camelcase-name-to-lisp-name name))))
                                                    :dat/parquet)
                         (parquet-struct-object)
                       (,@(mapcar (lambda (f)
                                    (let ((fdoc (parquet-struct-field-doc f))
                                          (fname (snakecase-name-to-lisp-name
                                                  (parquet-struct-field-name f))))
                                      `(,(intern fname :dat/parquet)
                                        ,@(when fdoc `(:documentation ,fdoc))
                                        :initarg ,(keywordicate fname)
                                        ;; TODO 2024-07-12: 
                                        ,@(when (equal "optional" (parquet-struct-field-required f))
                                            `(:initform nil))
                                        ,@(when-let ((ty (convert-parquet-struct-field-type f)))
                                            `(:type ,ty)))))
                                  fields))
                       ,@(when doc `((:documentation ,doc))))))))

(defmacro define-parquet-structs ()
  `(list
    ,@(%define-parquet-structs)))

(defmacro define-parquet-type (name opts &body body)
  "Define a parquet type with DEFTYPE which maps to LISP-TYPE."
  `(deftype ,(intern (concatenate 'string "PARQUET-" (substitute #\- #\_ name)) :dat/parquet) ,opts ,@body))

(defun define-parquet-types ()
  "Define all known values in *PARQUET-TYPES* using DEFINE-PARQUET-TYPE (DEFTYPE)."
  (list
   (define-parquet-type "BOOLEAN" () 'boolean)
   (define-parquet-type "INT32" () '(signed-byte 32))
   (define-parquet-type "INT64" () '(signed-byte 64))
   (define-parquet-type "INT96" () '(signed-byte 96))
   (define-parquet-type "FLOAT" () 'float)
   (define-parquet-type "DOUBLE" () 'double-float)
   (define-parquet-type "BYTE_ARRAY" (&optional size) `(octet-vector ,size))
   (define-parquet-type "FIXED_LEN_BYTE_ARRAY" (size) `(octet-vector ,size))))

(defun load-parquet (&key (file *parquet-json-file*))
  (init-parquet-json file)
  (with-package (:dat/parquet)
    (define-parquet-class parquet-struct-object () ())
    (let ((types (define-parquet-types)))
      (export types)
      (deftype dat/parquet::parquet-type (&optional (designator octet-vector) optional)
        (if optional
            (if (eql designator 'list)
                list
                `(or null ,designator))
            designator)))
    (export (mapcar 'class-name (define-parquet-structs)))
    (export *parquet-enums*)))
