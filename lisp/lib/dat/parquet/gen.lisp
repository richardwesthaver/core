;;; gen.lisp --- Parquet Lisp Code Generator

;; 

;;; Code:
(defpackage :dat/parquet/gen ;; not public API
  (:use :cl :std :dat/proto :dat/json)
  (:export :load-parquet))

(in-package :dat/parquet/gen)

(defparameter *parquet-json-file*
  (or (probe-file #.(asdf:system-relative-pathname :prelude #P"../.stash/parquet.json"))
      (warn "*PARQUET-JSON-FILE* not found")))

(defparameter *parquet-output-file*
  #.(asdf:system-relative-pathname :dat #P"parquet/thrift.lisp"))

(defvar *parquet-json* nil)

(eval-always
  (defun %parquet-json-enums ()
    (json-getf *parquet-json* "enums"))

  (defun dat/parquet::parquet-json-enum-getf (name)
    (json-getf
     (find-if (lambda (x) (equal name (json-getf x "name"))) (%parquet-json-enums))
     "members"))

  (defun dat/parquet::snakecase-name-to-lisp-name (string)
    (string-upcase
     (substitute #\- #\_ string)))

  (defun dat/parquet::camelcase-name-to-lisp-name (string)
    (string-upcase
     (with-output-to-string (name)
       (loop for i from 0 below (length string)
             for c across string
             when (and (upper-case-p c) (not (zerop i)))
               do (write-char #\- name)
             do (write-char c name))))))

(defvar *parquet-enums* nil)

(defmacro define-parquet-enum (sym name)
  `(progn
     (defvar ,(symbolicate "*PARQUET-JSON-" sym "*")
       ',(mapcar (lambda (x) (keywordicate (dat/parquet::snakecase-name-to-lisp-name (json-getf x "name"))))
                 (dat/parquet::parquet-json-enum-getf name)))))

(labels ((parse-type-id (type-id)
           (when type-id
             (string-case (type-id :default nil)
               ("bool" 'boolean)
               ("byte" 'signed-byte)
               ("i16" '(signed-byte 16))
               ("i32" '(signed-byte 32))
               ("i64" '(signed-byte 64))
               ("double" 'double-float)
               ("string" 'string)
               ("list" 'vector)
               ("binary" 'octet-vector)
               ("set" 'vector)
               ("enum" '(signed-byte 32))
               ("union" 'union)
               ("struct" 'struct))))
         (%intern (name)
           (if (stringp name)
               (symbolicate
                (cond 
                  ((equal name "UUIDType") "PARQUET-UUID-TYPE")
                  (t (concatenate 'string
                                  "PARQUET-"
                                  (dat/parquet::camelcase-name-to-lisp-name name)))))
               name))
         (parse-type (o)
           (when o
             (string-case ((json-getf o "typeId"))
               ("union" (%intern (json-getf o "class")))
               ("list"
                (if-let ((elt (json-getf o "elemType" nil)))
                  (%intern (parse-type elt))
                  (parse-type-id (json-getf o "elemTypeId"))))
               ("set"
                (if-let ((elt (json-getf o "elemType" nil)))
                  (%intern (parse-type elt))
                  (parse-type-id (json-getf o "elemTypeId"))))
               ("struct" (%intern (json-getf o "class")))
               ("enum" (%intern (json-getf o "class")))))))
  (defun convert-parquet-struct-field-type (field) ;; technically part of thrift type system
    (let* ((type-id (parse-type-id (parquet-struct-field-type-id field)))
           (type (parse-type (parquet-struct-field-type field)))
           (required (parquet-struct-field-required field)))
          (let ((ret (cond
                       ((eql 'vector type-id) `(vector ,type))
                       (t (or type type-id)))))
            (if (equal "optional" required)
                `(or null ,ret)
                ret)))))

(defparameter *parquet-structs* nil)

(defstruct (parquet-struct
            (:constructor make-parquet-struct (name doc exceptionp unionp fields)))
  name doc exceptionp unionp (fields nil :type list))

(defstruct (parquet-struct-field
            (:constructor make-parquet-struct-field (key name type-id type doc required)))
  key name type-id type doc required)

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

(eval-always
  (defun init-parquet-json (&optional (file *parquet-json-file*))
    (with-open-file (file file)
      (setq *parquet-json* (json-read file)))
    (setq *parquet-enums* (%parquet-json-enums))
    (setq *parquet-structs* (parquet-json-structs))))

;;; CLOS

;; (defmethod print-object ((obj parquet-object) stream)
;;   "Output a Parquet object to a stream."
;;   (print-unreadable-object (obj stream :type t)))

(defmacro define-parquet-class (name superclasses slots &rest options)
  "Define a new subclass of PARQUET-OBJECT with NAME."
  `(defclass ,name ,@(if-let ((s superclasses)) (list s) `((dat/parquet::parquet-object))) ,slots ,@options))

;;; Codegen

;; 8)
(eval-always
  (defun %define-parquet-structs ()
    "Define all known values in *PARQUET-STRUCTS* using DEFINE-PARQUET-CLASS (DEFCLASS)."
    (loop for struct in *parquet-structs*
          unless (null struct)
          collect (let* ((name (parquet-struct-name struct))
                         (doc (parquet-struct-doc struct))
                         (fields (parquet-struct-fields struct))
                         (class-name (symbolicate (cond
                                               ((equal name "UUIDType") "PARQUET-UUID-TYPE")
                                               (t (concatenate 'string
                                                               "PARQUET-"
                                                               (dat/parquet::camelcase-name-to-lisp-name name)))))))
                    `(progn
                       (defclass ,class-name (dat/parquet::parquet-object)
                                 (,@(mapcar (lambda (f)
                                              (let ((fdoc (parquet-struct-field-doc f))
                                                    (fname (dat/parquet::snakecase-name-to-lisp-name
                                                            (parquet-struct-field-name f))))
                                                `(,(symbolicate fname)
                                                  ,@(when fdoc `(:documentation ,fdoc))
                                                  :initarg ,(keywordicate fname)
                                                  ;; TODO 2024-07-12: 
                                                  ,@(when (equal "optional" (parquet-struct-field-required f))
                                                      `(:initform nil))
                                                  ,@(when-let ((ty (convert-parquet-struct-field-type f)))
                                                      `(:type ,ty)))))
                                            fields))
                                 ,@(when doc `((:documentation ,doc)))))))))

(defmacro define-parquet-type (name opts &body body)
  "Define a parquet type with DEFTYPE which maps to LISP-TYPE."
  `(progn (deftype ,(symbolicate "PARQUET-" (substitute #\- #\_ name)) ,opts ,@body)))

(defun parse-parquet-thrift-definitions (&key (input *parquet-json-file*)
                                           (output #.(asdf:system-relative-pathname :dat "parquet/thrift.lisp")))
  (init-parquet-json input)
  (with-open-file (defs output :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format defs ";;; ~a --- Parquet Thrift Definitions -*- buffer-read-only:t -*-

;; input = ~a

;; This file was generated automatically by
;; DAT/PARQUET/GEN:PARSE-PARQUET-THRIFT-DEFINITIONS

;; Do not modify.

;;; Code:
(in-package :dat/parquet)" output input)
    (format defs "~2%")
    (let ((enums   '((define-parquet-enum types "Type")
                     (define-parquet-enum converted-types "ConvertedType")
                     (define-parquet-enum field-repetition-types "FieldRepetitionType")
                     (define-parquet-enum encodings "Encoding")
                     (define-parquet-enum compression-codecs "CompressionCodec")
                     (define-parquet-enum page-types "PageType")
                     (define-parquet-enum boundary-orders "BoundaryOrder")))
          (types '((define-parquet-type "BOOLEAN" () 'boolean)
                   (define-parquet-type "INT32" () '(signed-byte 32))
                   (define-parquet-type "INT64" () '(signed-byte 64))
                   (define-parquet-type "INT96" () '(signed-byte 96))
                   (define-parquet-type "FLOAT" () 'float)
                   (define-parquet-type "DOUBLE" () 'double-float)
                   (define-parquet-type "BYTE_ARRAY" (&optional size) `(octet-vector ,size))
                   (define-parquet-type "FIXED_LEN_BYTE_ARRAY" (size) `(octet-vector ,size))))
          (structs (mapcar #'macroexpand-1 (%define-parquet-structs))))
      ;; expands to a progn, so we just take the cdr
      (dolist (en enums)
        (dolist (f (cdr (macroexpand en)))
          (write f :stream defs :case :downcase :readably t)
          (terpri defs)))
      (dolist (ty types)
        (dolist (f (cdr (macroexpand ty)))
          (write f :stream defs :case :downcase :readably t)
          (terpri defs)))
      (dolist (st structs)
        (dolist (f (cdr (macroexpand st)))
          (write f :stream defs :case :downcase :readably t)
          (terpri defs))))))
