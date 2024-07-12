;;; parquet.lisp --- Apache Parquet

;; Common Lisp implementation of Apache Parquet

;;; Commentary:

#|
https://github.com/apache/parquet-format
https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift
https://github.com/apache/parquet-testing
https://github.com/apache/parquet-java
https://github.com/apache/arrow-rs
|#

;; In this package we're being as lazy as possible. To generate our own
;; encoder/decoder methods we depend on the file parquet.thrift in the
;; parquet-format repo above. The core skelfile includes a script to download
;; it and convert it to parquet.json (requires the thirft cli tool). We then
;; decode it with DAT/JSON and visit all elements recursively, generating lisp
;; code using pre-compiled macros.

;; 
;;; Code:
(in-package :dat/parquet)

(eval-always
  (defparameter *default-parquet-json-file*
    (probe-file #.(asdf:system-relative-pathname :prelude #P"../.stash/parquet.json")))
  (defvar *parquet-json* nil)
  (defun load-parquet-json (&optional (json-file *default-parquet-json-file*))
    (with-open-file (file json-file)
      (setq *parquet-json* (json-read file))))
  (defun parquet-json-enums ()
    (json-getf *parquet-json* "enums"))  

  (defun parquet-json-enum-getf (name)
    (json-getf
     (find-if (lambda (x) (equal name (json-getf x "name"))) (parquet-json-enums))
     "members"))

  (defmacro def-parquet-enum (sym name)
    `(progn
       (defvar ,(symbolicate '*parquet- sym '*) nil)
       (defun ,(symbolicate 'parquet-json- sym) ()
         (mapcar (lambda (x) (json-getf x "name")) (parquet-json-enum-getf ,name)))))

  (defvar *parquet-structs* nil)
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
                                    (type (print (json-getf f "type")))
                                    (doc (json-getf f "doc"))
                                    (required (json-getf f "required")))
                                (make-parquet-struct-field key name type-id type doc required)))))
         (make-parquet-struct name doc exceptionp unionp fields) *parquet-structs*))
     (json-getf *parquet-json* "structs")))

  (defun parquet-json-namespaces ()
    (json-getf *parquet-json* "namespaces"))

  (defun init-parquet-json ()
    (load-parquet-json)
    (def-parquet-enum types "Type")
    (def-parquet-enum converted-types "ConvertedType")
    (def-parquet-enum field-repetition-types "FieldRepetitionType")
    (def-parquet-enum encodings "Encoding")
    (def-parquet-enum compression-codecs "CompressionCodec")
    (def-parquet-enum page-types "PageType")
    (def-parquet-enum boundary-orders "BoundaryOrder")
    (setq *parquet-structs* (parquet-json-structs))))

(eval-when (:compile-toplevel)
  (init-parquet-json))

(defclass parquet-object () ())

(defmethod print-object ((obj parquet-object) stream)
  "Output a Parquet object to a stream."
  (print-unreadable-object (obj stream :type t)
    (parquet-encode obj stream)))

(defmacro define-parquet-class (name superclasses slots &rest options)
  "Define a new subclass of PARQUET-OBJECT with NAME."
  `(defclass ,name ,(push 'parquet-object superclasses) ,slots ,@options))

(define-parquet-class logical-parquet-object () ())

(defgeneric parquet-read (value &optional stream))
(defgeneric parquet-write (value &optional stream))

(defmethod parquet-write ((value (eql t)) &optional stream)
  "Encode a parquet boolean true value."
  (declare (ignore value))
  (write-byte 1 stream))

(defmethod parquet-write ((value (eql nil)) &optional stream)
  "Encode a parquet boolean false value."
  (declare (ignore value))
  (write-byte 0 stream))

(defun parquet-encode (value &optional stream)
  "Encode a Lisp value and write it to a parquet stream."
  (parquet-write value stream))
