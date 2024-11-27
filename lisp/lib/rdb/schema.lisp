;;; schema.lisp --- RDB Schema Implementation

;; 

;;; Code:
(in-package :rdb)

(defclass rdb-schema (schema) ()
  (:default-initargs :fields (make-fields :default 'octet-vector)))

;; Note that we don't use SIMPLE-CONS-COLUMN here because the NAME slot of
;; columns is stored in the underlying RDB-CF structure object.
(defclass rdb-column (cons-column) ()
  (:default-initargs 
   :type (cons 'octet-vector 'octet-vector)))
